# Input/Output for CLISP
# Bruno Haible 1990-2001
# Marcus Daniels 11.3.1997

#include "lispbibl.c"
#include "arilev0.c" # for Division in pr_uint


# =============================================================================
# Readtable-functions
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
      {
        # Allocate the hash table.
        pushSTACK(S(Ktest)); pushSTACK(S(eq)); funcall(L(make_hash_table),2);
        pushSTACK(value1);
        # Allocate the simple-vector.
        var object table = allocate_vector(small_char_code_limit+1);
        TheSvector(table)->data[small_char_code_limit] = popSTACK();
        return table;
      }
    local object perchar_table_get (object table, chart c);
    local object perchar_table_get(table,c)
      var object table;
      var chart c;
      {
        if (as_cint(c) < small_char_code_limit) {
          return TheSvector(table)->data[as_cint(c)];
        } else {
          var object value = gethash(code_char(c),TheSvector(table)->data[small_char_code_limit]);
          return (eq(value,nullobj) ? NIL : value);
        }
      }
    local void perchar_table_put (object table, chart c, object value);
    local void perchar_table_put(table,c,value)
      var object table;
      var chart c;
      var object value;
      {
        if (as_cint(c) < small_char_code_limit) {
          TheSvector(table)->data[as_cint(c)] = value;
        } else {
          shifthash(TheSvector(table)->data[small_char_code_limit],code_char(c),value);
        }
      }
    local object copy_perchar_table (object table);
    local object copy_perchar_table(table)
      var object table;
      {
        pushSTACK(copy_svector(table));
        # Allocate a new hash table.
        pushSTACK(S(Ktest)); pushSTACK(S(eq)); funcall(L(make_hash_table),2);
        pushSTACK(value1);
        # stack layout: table, newht.
        map_hashtable(TheSvector(STACK_1)->data[small_char_code_limit],key,value,
                      { shifthash(STACK_(0+1),key,value); }
                     );
        var object newht = popSTACK();
        var object table = popSTACK();
        TheSvector(table)->data[small_char_code_limit] = newht;
        return table;
      }
  #else
    # A simple-vector of char_code_limit elements.
    #define allocate_perchar_table()  allocate_vector(char_code_limit)
    #define perchar_table_get(table,c)  TheSvector(table)->data[(uintP)as_cint(c)]
    #define perchar_table_put(table,c,value)  (TheSvector(table)->data[(uintP)as_cint(c)] = (value))
    #define copy_perchar_table(table)  copy_svector(table)
  #endif

# Construction of Readtables (cf. LISPBIBL.D):
  # readtable_syntax_table
  #    bitvector consisting of char_code_limit bytes: for each character the
  #                                                   syntaxcode is assigned
  # readtable_macro_table
  #    a vector with char_code_limit elements: for each character
  #    either  (if the character is no read-macro)
  #              NIL
  #    or        (if the character is a dispatch-macro)
  #              a vector with char_code_limit functions/NILs,
  #    or        (if the character is a miscellaneous read-macro)
  #              the function, which is called, when the character is read.
  # readtable_case
  #    a fixnum in {0,1,2,3}

# meaning of case (in sync with CONSTOBJ.D!):
  #define case_upcase    0
  #define case_downcase  1
  #define case_preserve  2
  #define case_invert    3

# meaning of the entries in the syntax_table:
  #define syntax_illegal      0  # invalid
  #define syntax_single_esc   1  # '\' (Single Escape)
  #define syntax_multi_esc    2  # '|' (Multiple Escape)
  #define syntax_constituent  3  # the rest (Constituent)
  #define syntax_whitespace   4  # TAB,LF,FF,CR,' ' (Whitespace)
  #define syntax_eof          5  # EOF
  #define syntax_t_macro      6  # '()'"' (Terminating Macro)
  #define syntax_nt_macro     7  # '#' (Non-Terminating Macro)
# <= syntax_constituent : if an object starts with such a character, it's a token.
#                         (ILL will deliver an error then.)
# >= syntax_t_macro : macro-character.
#                     if an object starts like that: call macro-funktion.

# Syntax tables, indexed by characters.
# allocate_syntax_table()
# syntax_table_get(table,c)
# syntax_table_put(table,c,value)
# can trigger GC
  #if (small_char_code_limit < char_code_limit)
    # A cons, consisting of a simple-bit-vector with small_char_code_limit
    # bytes, and a hash table mapping characters to fixnums. Characters not
    # found in the hash table are assumed to have the syntax code
    # (graphic_char_p(ch) ? syntax_constituent : syntax_illegal).
    local object allocate_syntax_table (void);
    local object allocate_syntax_table()
      {
        # Allocate the hash table.
        pushSTACK(S(Ktest)); pushSTACK(S(eq)); funcall(L(make_hash_table),2);
        pushSTACK(value1);
        # Allocate the simple-bit-vector.
        pushSTACK(allocate_bit_vector(Atype_8Bit,small_char_code_limit));
        var object new_cons = allocate_cons();
        Car(new_cons) = popSTACK();
        Cdr(new_cons) = popSTACK();
        return new_cons;
      }
    #define syntax_table_get(table,c)  \
      (as_cint(c) < small_char_code_limit           \
       ? TheSbvector(Car(table))->data[as_cint(c)] \
       : syntax_table_get_notinline(table,c)       \
      )
    local uintB syntax_table_get_notinline (object table, chart c);
    local uintB syntax_table_get_notinline(table,c)
      var object table;
      var chart c;
      {
        var object val = gethash(code_char(c),Cdr(table));
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
      {
        shifthash(Cdr(table),code_char(c),fixnum(value));
      }
  #else
    # A simple-bit-vector with char_code_limit bytes.
    #define allocate_syntax_table()  allocate_bit_vector(Atype_8Bit,char_code_limit)
    #define syntax_table_get(table,c)  TheSbvector(table)->data[as_cint(c)]
    #define syntax_table_put(table,c,value)  (TheSbvector(table)->data[as_cint(c)] = (value))
  #endif

# standard(original) syntaxtable(readtable)  for read characters:
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

# UP: returns the standard (original) readtable.
# orig_readtable()
# < result: standard(original) readtable
# can trigger GC
  local object orig_readtable (void);
  local object orig_readtable()
    {
      # initialize the syntax-table:
      {
        var object s_table = allocate_syntax_table(); # new bitvector
        pushSTACK(s_table); # save
        # and fill with the original:
        #if (small_char_code_limit < char_code_limit)
        s_table = Car(s_table);
        #endif
        var const uintB * ptr1 = &orig_syntax_table[0];
        var uintB* ptr2 = &TheSbvector(s_table)->data[0];
        var uintC count;
        dotimesC(count,small_char_code_limit, { *ptr2++ = *ptr1++; } );
      }
      # initialize dispatch-macro '#':
      {
        var object d_table = allocate_perchar_table(); # new vector
        pushSTACK(d_table); # save
        # and add the sub-character-functions for '#':
        var object* table = &TheSvector(d_table)->data[0];
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
      }
      # initialize READ-macros:
      {
        var object m_table = allocate_perchar_table(); # new Vektor, filled with NIL
        # and add the macro-characters:
        var object* table = &TheSvector(m_table)->data[0];
        table['('] = L(lpar_reader);
        table[')'] = L(rpar_reader);
        table['"'] = L(string_reader);
        table['\''] = L(quote_reader);
        table['#'] = popSTACK(); # dispatch-vector for '#'
        table[';'] = L(line_comment_reader);
        table['`'] = S(backquote_reader); # cf. BACKQUOTE.LISP
        table[','] = S(comma_reader); # cf. BACKQUOTE.LISP
        pushSTACK(m_table); # save
      }
      # build readtable:
      {
        var object readtable = allocate_readtable(); # new readtable
        TheReadtable(readtable)->readtable_macro_table = popSTACK(); # m_table
        TheReadtable(readtable)->readtable_syntax_table = popSTACK(); # s_table
        TheReadtable(readtable)->readtable_case = fixnum(case_upcase); # :UPCASE
        return readtable;
      }
    }

# UP: copies a readtable
# copy_readtable_contents(from_readtable,to_readtable)
# > from-readtable
# > to-readtable
# < result : to-Readtable with same content
# can trigger GC
  local object copy_readtable_contents (object from_readtable, object to_readtable);
  local object copy_readtable_contents(from_readtable,to_readtable)
    var object from_readtable;
    var object to_readtable;
    {
      # copy the case-slot:
      TheReadtable(to_readtable)->readtable_case = TheReadtable(from_readtable)->readtable_case;
      # copy the syntaxtable:
      {
        var object stable1;
        var object stable2;
        #if (small_char_code_limit < char_code_limit)
          pushSTACK(to_readtable);
          pushSTACK(from_readtable);
          # Allocate a new hash table.
          pushSTACK(S(Ktest)); pushSTACK(S(eq)); funcall(L(make_hash_table),2);
          pushSTACK(value1);
          # stack layout: to-readtable, from-readtable, newht.
          map_hashtable(Cdr(TheReadtable(STACK_1)->readtable_syntax_table),ch,entry, {
                          shifthash(STACK_(0+1),ch,entry);
                        });
          {
            var object newht = popSTACK();
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
        var const uintB* ptr1 = &TheSbvector(stable1)->data[0];
        var uintB* ptr2 = &TheSbvector(stable2)->data[0];
        var uintC count;
        dotimesC(count,small_char_code_limit, { *ptr2++ = *ptr1++; } );
      }
      # copy the macro-table:
      pushSTACK(to_readtable); # save to-readtable
      {
        var object mtable1 = TheReadtable(from_readtable)->readtable_macro_table;
        var object mtable2 = TheReadtable(to_readtable)->readtable_macro_table;
        var uintL i;
        for (i = 0; i < small_char_code_limit; i++) {
          # copy entry number i:
          var object entry = TheSvector(mtable1)->data[i];
          if (simple_vector_p(entry)) {
            # simple-vector is copied element for element:
            pushSTACK(mtable1); pushSTACK(mtable2);
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
          # stack layout: mtable2, newht.
          map_hashtable(TheSvector(mtable1)->data[small_char_code_limit],ch,entry, {
                          if (simple_vector_p(entry))
                            entry = copy_perchar_table(entry);
                          shifthash(STACK_(0+1),ch,entry);
                        });
          TheSvector(STACK_1)->data[small_char_code_limit] = STACK_0;
          skipSTACK(2);
        #endif
      }
      return popSTACK(); # to-readtable as result
    }

# UP: copies a readtable
# copy_readtable(readtable)
# > readtable: Readtable
# < result: copy of readtable, semantically equivalent
# can trigger GC
  local object copy_readtable (object from_readtable);
  local object copy_readtable(from_readtable)
    var object from_readtable;
    {
      pushSTACK(from_readtable); # save
      pushSTACK(allocate_syntax_table()); # new empty syntaxtable
      pushSTACK(allocate_perchar_table()); # new empty macro-table
      var object to_readtable = allocate_readtable(); # new readtable
      # fill:
      TheReadtable(to_readtable)->readtable_macro_table = popSTACK();
      TheReadtable(to_readtable)->readtable_syntax_table = popSTACK();
      # and copy content:
      return copy_readtable_contents(popSTACK(),to_readtable);
    }

# error at wrong value of *READTABLE*
# fehler_bad_readtable(); english: error_bad_readtable();
  nonreturning_function(local, fehler_bad_readtable, (void));
  local void fehler_bad_readtable()
    {
      # correct *READTABLE*:
      var object sym = S(readtablestern); # Symbol *READTABLE*
      var object oldvalue = Symbol_value(sym);
      Symbol_value(sym) = O(standard_readtable); # := Standard-Readtable of Common Lisp
      # and report the error:
      pushSTACK(oldvalue);     # TYPE-ERROR slot DATUM
      pushSTACK(S(readtable)); # TYPE-ERROR slot EXPECTED-TYPE
      pushSTACK(sym);
      fehler(type_error,
             GETTEXT("The value of ~ was not a readtable. It has been reset.")
            );
    }

# Macro: fetches the current readtable. argument 'zuweisung' means 'assignment'
# get_readtable(readtable =);
# < readtable : the current readtable
  #if 0
    #define get_readtable(zuweisung)  \
      { if (!readtablep(Symbol_value(S(readtablestern)))) { fehler_bad_readtable(); }  \
        zuweisung Symbol_value(S(readtablestern));                                     \
      }
  #else # oder (optimized):
    #define get_readtable(zuweisung)  \
      { if (!(orecordp(Symbol_value(S(readtablestern)))                                          \
              && (Record_type( zuweisung Symbol_value(S(readtablestern)) ) == Rectype_Readtable) \
           ) )                                                                                   \
          { fehler_bad_readtable(); }                                                            \
      }
  #endif


# =============================================================================
# Initialization
# =============================================================================

# UP: Initializes the reader.
# init_reader();
# can trigger GC
  global void init_reader (void);
  global void init_reader()
    {
      # initialize *READ-BASE*:
        define_variable(S(read_base),fixnum(10)); # *READ-BASE* := 10
      # initialize *READ-SUPPRESS*:
        define_variable(S(read_suppress),NIL);    # *READ-SUPPRESS* := NIL
      # initialize *READ-EVAL*:
        define_variable(S(read_eval),T);          # *READ-EVAL* := T
      # initialize *READTABLE*:
      {
        var object readtable = orig_readtable();
        O(standard_readtable) = readtable; # that is the standard-readtable,
        readtable = copy_readtable(readtable); # one copy of it
        define_variable(S(readtablestern),readtable);   # =: *READTABLE*
      }
      # initialize token_buff_1 and token_buff_2:
        O(token_buff_1) = NIL;
        # token_buff_1 and token_buff_2 will be initialized
        # with a semi-simple-string and a semi-simple-byte-vector
        # at the first call of get_buffers (see below).
      # Displaced-String initialisieren:
        # new array (with data-vector NIL), Displaced, rank=1
        O(displaced_string) =
          allocate_iarray(bit(arrayflags_displaced_bit)|bit(arrayflags_dispoffset_bit)|
                          Atype_Char,
                          1,
                          Array_type_string
                         );
    }

LISPFUNN(defio,2)
# (SYS::%DEFIO dispatch-reader vector-index) post-initialises the I/O.
  {
    O(dispatch_reader) = STACK_1;
    O(dispatch_reader_index) = STACK_0;
    value1 = NIL; mv_count=0; skipSTACK(2);
  }


# =============================================================================
# LISP - Functions for readtables
# =============================================================================

# error, if argument is no Readtable.
# fehler_readtable(obj);  means: error_readtable(obj);
# > obj: erroneous Argument
# > subr_self: caller (a SUBR)
  nonreturning_function(local, fehler_readtable, (object obj));
  local void fehler_readtable(obj)
    var object obj;
    {
      pushSTACK(obj);          # TYPE-ERROR slot DATUM
      pushSTACK(S(readtable)); # TYPE-ERROR slot EXPECTED-TYPE
      pushSTACK(obj);
      pushSTACK(TheSubr(subr_self)->name);
      fehler(type_error,
             GETTEXT("~: argument ~ is not a readtable")
            );
    }
#define CHECK_READTABLE(obj)  if (!readtablep(obj)) fehler_readtable(obj)

LISPFUN(copy_readtable,0,2,norest,nokey,0,NIL)
# (COPY-READTABLE [from-readtable [to-readtable]]), CLTL p. 361
  {
    var object from_readtable = STACK_1;
    if (eq(from_readtable,unbound)) {
      # no arguments are given
      get_readtable(from_readtable=); # current readtable
      value1 = copy_readtable(from_readtable); # copy
    } else {
      if (nullp(from_readtable)) {
        # instead of  NIL take the standard-readtable
        from_readtable = O(standard_readtable);
      } else {
        # check from-readtable:
        CHECK_READTABLE(from_readtable);
      }
      # from-readtable is OK
      var object to_readtable = STACK_0;
      if (eq(to_readtable,unbound) || nullp(to_readtable)) {
        # copy from-readtable, without to-readtable
        value1 = copy_readtable(from_readtable);
      } else {
        # check to-readtable and copy it:
        CHECK_READTABLE(to_readtable);
        value1 = copy_readtable_contents(from_readtable,to_readtable);
      }
    }
    mv_count=1; skipSTACK(2);
  }

LISPFUN(set_syntax_from_char,2,2,norest,nokey,0,NIL)
# (SET-SYNTAX-FROM-CHAR to-char from-char [to-readtable [from-readtable]]),
# CLTL p. 361
  {
    var object to_char = STACK_3;
    var object from_char = STACK_2;
    var object to_readtable = STACK_1;
    var object from_readtable = STACK_0;
    # check to-char:
    if (!charp(to_char)) # must be a character
      fehler_char(to_char);
    # check from-char:
    if (!charp(from_char)) # must be a character
      fehler_char(from_char);
    # check to-readtable:
    if (eq(to_readtable,unbound)) {
      get_readtable(to_readtable=); # default is the current readtable
    } else {
      CHECK_READTABLE(to_readtable);
    }
    # check from-readtable:
    if (eq(from_readtable,unbound) || nullp(from_readtable)) {
      from_readtable = O(standard_readtable); # default is the standard-readtable
    } else {
      CHECK_READTABLE(from_readtable);
    }
    STACK_1 = to_readtable;
    STACK_0 = from_readtable;
    # now to_char, from_char, to_readtable, from_readtable are OK.
    {
      var chart to_c = char_code(to_char);
      var chart from_c = char_code(from_char);
      # copy syntaxcode:
      syntax_table_put(TheReadtable(to_readtable)->readtable_syntax_table,to_c,
        syntax_table_get(TheReadtable(from_readtable)->readtable_syntax_table,from_c));
      # copy macro-function/vector:
      var object entry = perchar_table_get(TheReadtable(STACK_0)->readtable_macro_table,from_c);
      if (simple_vector_p(entry))
        # if entry is a simple-vector, it must be copied:
        { entry = copy_perchar_table(entry); }
      perchar_table_put(TheReadtable(STACK_1)->readtable_macro_table,to_c,entry);
    }
    value1 = T; mv_count=1; # value T
    skipSTACK(4);
  }

# UP: checks an optional readtable-argument,
# with default = current readtable.
# > STACK_0: Argument
# > subr_self: caller (a SUBR)
# < STACK: increased by 1
# < result: readtable
  local object test_readtable_arg (void);
  local object test_readtable_arg()
    {
      var object readtable = popSTACK();
      if (eq(readtable,unbound)) {
        get_readtable(readtable=); # the current readtable is default
      } else {
        CHECK_READTABLE(readtable);
      }
      return readtable;
    }

# UP: checks an optional readtable-argument,
# with default = current readtable, nil = standard-readtable.
# > STACK_0: Argument
# > subr_self: caller (a SUBR)
# < STACK: increased by 1
# < result: readtable
  local object test_readtable_null_arg (void);
  local object test_readtable_null_arg()
    {
      var object readtable = popSTACK();
      if (eq(readtable,unbound)) {
        get_readtable(readtable=); # the current readtable is default
      } elif (nullp(readtable)) {
        readtable = O(standard_readtable); # respectively the standard-readtable
      } else {
        CHECK_READTABLE(readtable);
      }
      return readtable;
    }

# UP: checks the next-to-last optional argument of
# SET-MACRO-CHARACTER and MAKE-DISPATCH-MACRO-CHARACTER.
# > STACK_0: non-terminating-p - Argument
# > subr_self: caller (a SUBR)
# < STACK: increased by 1
# < result: new syntaxcode
  local uintB test_nontermp_arg (void);
  local uintB test_nontermp_arg()
    {
      var object arg = popSTACK();
      if (eq(arg,unbound) || nullp(arg))
        return syntax_t_macro; # terminating is default
      else
        return syntax_nt_macro; # non-terminating-p given and /= NIL
    }

LISPFUN(set_macro_character,2,2,norest,nokey,0,NIL)
# (SET-MACRO-CHARACTER char function [non-terminating-p [readtable]]),
# CLTL p. 362
  {
    # check char:
    {
      var object ch = STACK_3;
      if (!charp(ch))
        fehler_char(ch);
    }
    # check function and convert into an object of type FUNCTION:
    {
      var object function = coerce_function(STACK_2);
      if (cclosurep(function)
          && eq(TheCclosure(function)->clos_codevec,TheCclosure(O(dispatch_reader))->clos_codevec)) {
        var object vector =
          ((Srecord)TheCclosure(function))->recdata[posfixnum_to_L(O(dispatch_reader_index))];
        if (simple_vector_p(vector)) {
          # It's a clone of #'dispatch-reader. Pull out the vector.
          function = copy_perchar_table(vector);
        }
      }
      STACK_2 = function;
    }
    var object readtable = test_readtable_arg(); # Readtable
    var uintB syntaxcode = test_nontermp_arg(); # new syntaxcode
    var chart c = char_code(STACK_1);
    STACK_1 = readtable;
    # set syntaxcode:
    syntax_table_put(TheReadtable(readtable)->readtable_syntax_table,c,syntaxcode);
    # add macrodefinition:
    perchar_table_put(TheReadtable(STACK_1)->readtable_macro_table,c,STACK_0);
    value1 = T; mv_count=1; # 1 value T
    skipSTACK(2);
  }

LISPFUN(get_macro_character,1,1,norest,nokey,0,NIL)
# (GET-MACRO-CHARACTER char [readtable]), CLTL p. 362
  {
    # check char:
    {
      var object ch = STACK_1;
      if (!charp(ch))
        fehler_char(ch);
    }
    var object readtable = test_readtable_null_arg(); # Readtable
    var object ch = popSTACK();
    var chart c = char_code(ch);
    # Test the Syntaxcode:
    var object nontermp = NIL; # non-terminating-p Flag
    switch (syntax_table_get(TheReadtable(readtable)->readtable_syntax_table,c)) {
      case syntax_nt_macro: nontermp = T;
      case syntax_t_macro: # nontermp = NIL;
        # c is a macro-character.
        {
          var object entry = perchar_table_get(TheReadtable(readtable)->readtable_macro_table,c);
          if (simple_vector_p(entry)) {
            # c is a dispatch-macro-character.
            if (nullp(O(dispatch_reader))) {
              # Shouldn't happen (bootstrapping problem).
              pushSTACK(ch);
              pushSTACK(TheSubr(subr_self)->name);
              fehler(error,
                     GETTEXT("~: ~ is a dispatch macro character")
                    );
            }
            # Clone #'dispatch-reader.
            pushSTACK(copy_perchar_table(entry));
            var object newclos = allocate_cclosure_copy(O(dispatch_reader));
            do_cclosure_copy(newclos,O(dispatch_reader));
            ((Srecord)TheCclosure(newclos))->recdata[posfixnum_to_L(O(dispatch_reader_index))] = popSTACK();
            value1 = newclos;
          } else {
            value1 = entry;
          }
        }
        break;
      default: # nontermp = NIL;
        value1 = NIL; break;
    }
    value2 = nontermp; mv_count=2; # nontermp as second value
  }

LISPFUN(make_dispatch_macro_character,1,2,norest,nokey,0,NIL)
# (MAKE-DISPATCH-MACRO-CHARACTER char [non-terminating-p [readtable]]),
# CLTL p. 363
  {
    var object readtable = test_readtable_arg(); # Readtable
    var uintB syntaxcode = test_nontermp_arg(); # new syntaxcode
    # check char:
    var object ch = popSTACK();
    if (!charp(ch))
      fehler_char(ch);
    var chart c = char_code(ch);
    # fetch new (empty) dispatch-macro-table:
    pushSTACK(readtable);
    pushSTACK(allocate_perchar_table()); # vector, filled with NIL
    # store everything in the readtable:
    # syntaxcode into syntax-table:
    syntax_table_put(TheReadtable(STACK_1)->readtable_syntax_table,c,syntaxcode);
    # new dispatch-macro-table into the  macrodefinitionen-table:
    perchar_table_put(TheReadtable(STACK_1)->readtable_macro_table,c,STACK_0);
    value1 = T; mv_count=1; # 1 value T
    skipSTACK(2);
  }

# UP: checks the arguments disp-char and sub-char.
# > STACK: STACK_1 = disp-char, STACK_0 = sub-char
# > readtable: Readtable
# > subr_self: caller (a SUBR)
# < result: the dispatch-macro-table for disp-char,
#             nullobj if sub-char is a digit.
  local object test_disp_sub_char (object readtable);
  local object test_disp_sub_char(readtable)
    var object readtable;
    {
      var object sub_ch = STACK_0; # sub-char
      var object disp_ch = STACK_1; # disp-char
      if (!charp(disp_ch)) # disp-char must be a character
        fehler_char(disp_ch);
      if (!charp(sub_ch)) # sub-char must be a character
        fehler_char(sub_ch);
      var chart disp_c = char_code(disp_ch);
      var object entry = perchar_table_get(TheReadtable(readtable)->readtable_macro_table,disp_c);
      if (!simple_vector_p(entry)) {
        pushSTACK(disp_ch);
        pushSTACK(TheSubr(subr_self)->name);
        fehler(error,
               GETTEXT("~: ~ is not a dispatch macro character")
              );
      }
      # disp-char is a dispatching-macro-character, entry is the vector.
      var cint sub_c = as_cint(up_case(char_code(sub_ch))); # convert sub-char into capitals
      if ((sub_c >= '0') && (sub_c <= '9'))
        # digit
        return nullobj;
      else
        # valid sub-char
        return entry;
    }

LISPFUN(set_dispatch_macro_character,3,1,norest,nokey,0,NIL)
# (SET-DISPATCH-MACRO-CHARACTER disp-char sub-char function [readtable]),
# CLTL p. 364
  {
    # check function and convert it into an object of Type FUNCTION:
    STACK_1 = coerce_function(STACK_1);
    subr_self = L(set_dispatch_macro_character);
    var object readtable = test_readtable_arg(); # Readtable
    var object function = popSTACK(); # function
    var object dm_table = test_disp_sub_char(readtable);
    if (eq(dm_table,nullobj)) {
      # STACK_0 = sub-char, TYPE-ERROR slot DATUM
      pushSTACK(O(type_not_digit)); # TYPE-ERROR slot EXPECTED-TYPE
      pushSTACK(STACK_1);
      pushSTACK(TheSubr(subr_self)->name);
      fehler(type_error,
             GETTEXT("~: digit $ not allowed as sub-char")
            );
    } else {
      perchar_table_put(dm_table,up_case(char_code(STACK_0)),function); # add function to the dispatch-macro-table
      value1 = T; mv_count=1; skipSTACK(2); # 1 Wert T
    }
  }

LISPFUN(get_dispatch_macro_character,2,1,norest,nokey,0,NIL)
# (GET-DISPATCH-MACRO-CHARACTER disp-char sub-char [readtable]), CLTL p. 364
  {
    var object readtable = test_readtable_null_arg(); # Readtable
    var object dm_table = test_disp_sub_char(readtable);
    value1 = (eq(dm_table,nullobj) ? NIL : perchar_table_get(dm_table,up_case(char_code(STACK_0)))); # NIL or Function as value
    mv_count=1; skipSTACK(2);
  }

LISPFUNN(readtable_case,1)
# (READTABLE-CASE readtable), CLTL2 S. 549
  {
    var object readtable = popSTACK(); # Readtable
    CHECK_READTABLE(readtable);
    value1 = (&O(rtcase_0))[(uintW)posfixnum_to_L(TheReadtable(readtable)->readtable_case)];
    mv_count=1;
  }

LISPFUNN(set_readtable_case,2)
# (SYSTEM::SET-READTABLE-CASE readtable value), CLTL2 p. 549
  {
    var object value = popSTACK();
    var object readtable = popSTACK(); # Readtable
    CHECK_READTABLE(readtable);
    # convert symbol value into an index by searching in table O(rtcase..):
    var const object* ptr = &O(rtcase_0);
    var object rtcase = Fixnum_0;
    var uintC count;
    dotimesC(count,4, {
      if (eq(*ptr,value))
        goto found;
      ptr++; rtcase = fixnum_inc(rtcase,1);
    });
    # invalid value
    pushSTACK(value);          # TYPE-ERROR slot DATUM
    pushSTACK(O(type_rtcase)); # TYPE-ERROR slot EXPECTED-TYPE
    pushSTACK(O(rtcase_3)); pushSTACK(O(rtcase_2)); pushSTACK(O(rtcase_1)); pushSTACK(O(rtcase_0));
    pushSTACK(value);
    pushSTACK(S(set_readtable_case));
    fehler(type_error,
           GETTEXT("~: new value ~ should be ~, ~, ~ or ~.")
          );
   found: # found in  table
    TheReadtable(readtable)->readtable_case = rtcase;
    value1 = value; mv_count=1;
  }

# =============================================================================
# some auxiliary routins and  macros for READ and PRINT
# =============================================================================

# Tests the dynamic value of a  symbols being /=NIL
# < true, if /= NIL
# #define test_value(sym)  (!nullp(Symbol_value(sym)))
  #define test_value(sym)  (!eq(NIL,Symbol_value(sym)))

# UP: fetches the value of a symbol. must be fixnum >=2, <=36.
# get_base(symbol)
# > symbol: Symbol
# < result: value of the Symbols, >=2, <=36.
  local uintL get_base (object symbol);
  local uintL get_base(symbol)
    var object symbol;
    {
      var object value = Symbol_value(symbol);
      var uintL wert;
      if (posfixnump(value) &&
          (wert = posfixnum_to_L(value), ((wert >= 2) && (wert <= 36)))
         ) {
        return wert;
      } else {
        Symbol_value(symbol) = fixnum(10);
        pushSTACK(value);         # TYPE-ERROR slot DATUM
        pushSTACK(O(type_radix)); # TYPE-ERROR slot EXPECTED-TYPE
        pushSTACK(value);
        pushSTACK(symbol);
        fehler(type_error,
               GETTEXT("The value of ~ should be an integer between 2 and 36, not ~." NLstring
                       "It has been reset to 10.")
              );
      }
    }

# UP: fetches the value of *PRINT-BASE*
# get_print_base()
# < uintL result: >=2, <=36
  #define get_print_base()  \
    (test_value(S(print_readably)) ? 10 : get_base(S(print_base)))

# UP: fetches the value of *READ-BASE*
# get_read_base()
# < uintL ergebnis: >=2, <=36
  #define get_read_base()  get_base(S(read_base))


# =============================================================================
#                              R E A D
# =============================================================================

# Single characters are read.
# The syntaxcodes (compare CLTL Table 22-1) are determined by use of the readtable.
# An (Extended) token is intercepted when syntaxcode = constituent .
# An attribut a_xxxx is allocated to every character in the token
# by use of the attribute-table (compare CLTL Table 22-3).
# O(token_buff_1) is a semi-simple-string, which contains the characters of
# the currently read extended-token.
# O(token_buff_2) is a semi-simple-byte-vektor, which contains the attributs of
# the currently read extended-token.
# Both have the same length (in characters respectively bytes).

# special objects, that can be returned by READ:
#   eof_value: special object, that indicates EOF
#   dot_value: auxiliary value for the detection of single dots

# ------------------------ READ on character-level ---------------------------

# error, if read object is a character:
# fehler_charread(ch,&stream);  english: error_charread(ch,&stream);
  nonreturning_function(local, fehler_charread, (object ch, const object* stream_));
  local void fehler_charread(ch,stream_)
    var object ch;
    var const object* stream_;
    {
      pushSTACK(*stream_); # value for slot STREAM of STREAM-ERROR
      pushSTACK(ch); # Character
      pushSTACK(*stream_); # Stream
      pushSTACK(S(read));
      fehler(stream_error,
             GETTEXT("~ from ~: character read should be a character: ~")
            );
    }

# UP: Reads a character and calculates its syntaxcode.
# read_char_syntax(ch=,scode=,&stream);
# > stream: Stream
# < stream: Stream
# < object ch: Character or eof_value
# < uintWL scode: Syntaxcode (from the current readtable) respectively syntax_eof
# "zuweisung" = "assignment"
# can trigger GC
  #define read_char_syntax(ch_zuweisung,scode_zuweisung,stream_)  \
    { var object ch0 = read_char(stream_); # read character            \
      ch_zuweisung ch0;                                                \
      if (eq(ch0,eof_value)) # EOF ?                                   \
        { scode_zuweisung syntax_eof; }                                \
        else                                                           \
        { # check for character:                            \
          if (!charp(ch0)) { fehler_charread(ch0,stream_); }           \
         {var object readtable;                                        \
          get_readtable(readtable = );                                 \
          scode_zuweisung # fetch syntaxcode from table                \
            syntax_table_get(TheReadtable(readtable)->readtable_syntax_table,char_code(ch0)); \
        }}                                                             \
    }

# error-message at EOF outside of objects
# fehler_eof_aussen(&stream); english: error_eof_outside(&stream);
# > stream: Stream
  nonreturning_function(local, fehler_eof_aussen, (const object* stream_));
  local void fehler_eof_aussen(stream_)
    var const object* stream_;
    {
      pushSTACK(*stream_); # value for slot STREAM of STREAM-ERROR
      pushSTACK(*stream_); # Stream
      pushSTACK(S(read));
      fehler(end_of_file,
             GETTEXT("~: input stream ~ has reached its end")
            );
    }

# error-message at EOF inside of objects
# fehler_eof_innen(&stream);  english: error_eof_inside(&stream)
# > stream: Stream
  nonreturning_function(local, fehler_eof_innen, (const object* stream_));
  local void fehler_eof_innen(stream_)
    var const object* stream_;
    {
      pushSTACK(*stream_); # value for slot STREAM of STREAM-ERROR
      if (posfixnump(Symbol_value(S(read_line_number)))) { # check SYS::*READ-LINE-NUMBER*
        pushSTACK(Symbol_value(S(read_line_number))); # line-number
        pushSTACK(*stream_); # Stream
        pushSTACK(S(read));
        fehler(end_of_file,
               GETTEXT("~: input stream ~ ends within an object. Last opening parenthesis probably in line ~.")
              );
      } else {
        pushSTACK(*stream_); # Stream
        pushSTACK(S(read));
        fehler(end_of_file,
               GETTEXT("~: input stream ~ ends within an object")
              );
      }
    }

# error-message at EOF, according to *READ-RECURSIVE-P*
# fehler_eof(&stream); english: error_eof(&stream)
# > stream: Stream
  nonreturning_function(local, fehler_eof, (const object* stream_));
  local void fehler_eof(stream_)
    var const object* stream_;
    {
      if (test_value(S(read_recursive_p))) # *READ-RECURSIVE-P* /= NIL ?
        fehler_eof_innen(stream_);
      else
        fehler_eof_aussen(stream_);
    }

# UP: read up to the next non-whitespace-character, without consuming it
# At EOF --> Error.
# wpeek_char_syntax(ch=,scode=,&stream);
# > stream: Stream
# < stream: Stream
# < object ch: next character
# < uintWL scode: its syntaxcode
# can trigger GC
  #define wpeek_char_syntax(ch_zuweisung,scode_zuweisung,stream_)  \
    { loop                                                                 \
        { var object ch0 = read_char(stream_); # read Character            \
          if (eq(ch0,eof_value)) { fehler_eof(stream_); } # EOF -> Error   \
          # check for Character:                                           \
          if (!charp(ch0)) { fehler_charread(ch0,stream_); }               \
          {var object readtable;                                           \
           get_readtable(readtable = );                                    \
           if (!((scode_zuweisung # fetch Syntaxcode from table            \
                    syntax_table_get(TheReadtable(readtable)->readtable_syntax_table,char_code(ch0)) \
                 )                                                         \
                 == syntax_whitespace                                      \
              ) )                                                          \
             # no Whitespace -> push back last read character              \
             { unread_char(stream_,ch0); ch_zuweisung ch0; break; }        \
        } }                                                                \
    }

# UP: read up to the next non-whitespace-character, without consuming it.
# wpeek_char_eof(&stream)
# > stream: Stream
# < stream: Stream
# < result: next character or eof_value
# can trigger GC
  local object wpeek_char_eof (const object* stream_);
  local object wpeek_char_eof(stream_)
    var const object* stream_;
    {
      loop {
        var object ch = read_char(stream_); # read character
        if (eq(ch,eof_value)) # EOF ?
          return ch;
        # check for Character:
        if (!charp(ch))
          fehler_charread(ch,stream_);
        var object readtable;
        get_readtable(readtable = );
        if (!(( # fetch Syntaxcode from table
               syntax_table_get(TheReadtable(readtable)->readtable_syntax_table,char_code(ch))
              )
              == syntax_whitespace
           ) ) {
          # no Whitespace -> push back last read character
          unread_char(stream_,ch); return ch;
        }
      }
    }

# ------------------------ READ at token-level -------------------------------

# read_token and test_potential_number_syntax, test_number_syntax need
# the attributes according to table 22-3.
# During test_potential_number_syntax attributes are transformed,
# a_digit partially into a_alpha or a_letter or a_expo_m.

# meaning of the entries in attribute_table:
  #define a_illg     0   # illegal constituent
  #define a_pack_m   1   # ':' = Package-marker
  #define a_alpha    2   # character without special property (alphabetic)
  #define a_escaped  3   # character without special property, case can not be converted
  #define a_ratio    4   # '/'
  #define a_dot      5   # '.'
  #define a_plus     6   # '+'
  #define a_minus    7   # '-'
  #define a_extens   8   # '_^' extension characters
  #define a_digit    9   # '0123456789'
  #define a_letter  10   # 'A'-'Z','a'-'z', not 'esfdlESFDL'
  #define a_expo_m  11   # 'esfdlESFDL'
  #    >= a_letter       #  'A'-'Z','a'-'z'
  #    >= a_digit        # '0123456789','A'-'Z','a'-'z'
  #    >= a_ratio        # what a potential number must consist of

# attribute-table for constituents, first interpretation:
# note: first, 0-9,A-Z,a-z are interpreted as a_digit or a_expo_m,
# then (if no integer can be deduced out of token), a_digit
# is interpreted as a_alpha (alphabetic) above of *READ-BASE*.
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

# Flag. indicates, if  single-escape- or multiple-escape-character
# occurred in the last read token:
  local bool token_escape_flag;

# UP: delivers two Buffers.
# if two buffers are available in the reservoir O(token_buff_1), O(token_buff_2),
# they are extracted. Otherwise new ones are allocated.
# If the buffers are not needed anymore, they can be written in
# O(token_buff_1) and O(token_buff_2).
# < STACK_1: a Semi-Simple String with Fill-Pointer 0
# < STACK_0: a Semi-Simple Byte-Vector with Fill-Pointer 0
# < STACK: decreased by 2
# can trigger GC
  local void get_buffers (void);
  local void get_buffers()
    { # Mechanism:
      # O(token_buff_1) and O(token_buff_2) hold a Semi-Simple-String
      # and a Semi-Simple-Byte-Vector, which are extracted if necessary (and marked
      # with O(token_buff_1) := NIL as extracted)
      # After use, they can be stored back again. Reentrant!
      var object buff_1 = O(token_buff_1);
      if (!nullp(buff_1)) {
        # extract buffer and empty:
        TheIarray(buff_1)->dims[1] = 0; # Fill-Pointer:=0
        pushSTACK(buff_1); # 1. Buffer finished
        var object buff_2 = O(token_buff_2);
        TheIarray(buff_2)->dims[1] = 0; # Fill-Pointer:=0
        pushSTACK(buff_2); # 2. Buffer finished
        O(token_buff_1) = NIL; # mark buffer as extracted
      } else {
        # buffers are extracted and must be allocated newly:
        pushSTACK(make_ssstring(50)); # new Semi-Simple-String with Fill-Pointer=0
        pushSTACK(make_ssbvector(50)); # new Semi-Simple-Byte-Vector with Fill-Pointer=0
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

# UP: reads an extended token, first character has already been read.
# read_token_1(&stream,ch,scode);
# > stream: Stream
# > ch, scode: first character and its syntaxcode
# < stream: Stream
# < O(token_buff_1): read characters
# < O(token_buff_2): their attributcodes
# < token_escape_flag: Escape-character-Flag
# can trigger GC
  local void read_token_1 (const object* stream_, object ch, uintWL scode);

  local void read_token(stream_)
    var const object* stream_;
    {
      # read first character:
      var object ch;
      var uintWL scode;
      read_char_syntax(ch = ,scode = ,stream_);
      # build up token:
      read_token_1(stream_,ch,scode);
    }

  local void read_token_1(stream_,ch,scode)
    var const object* stream_;
    var object ch;
    var uintWL scode;
    {
      # fetch empty Token-Buffers, upon STACK:
      get_buffers(); # (don't need to save ch)
      # the two buffers lie up th the end of read_token_1 in the Stack.
      # (thus read_char can call read recursively...)
      # Afterwards (during test_potential_number_syntax, test_number_syntax,
      # test_dots, read_internal up to the end of read_internal)
      # the buffers lie in O(token_buff_1) and O(token_buff_2). After the return of
      # read_internal their content is useless, and they can be used for further
      # read-operations.
      var bool multiple_escape_flag = false;
      var bool escape_flag = false;
      goto char_read;
      loop {
        # Here the token in STACK_1 (Semi-Simple-String for characters)
        # and STACK_0 (Semi-Simple-Byte-Vector for attributecodes) is constructed.
        # Multiple-Escape-Flag indicates, if we are situated between |...|.
        # Escape-Flag indicates, if a Escape-Character has appeared.
        read_char_syntax(ch = ,scode = ,stream_); # read next character
       char_read:
        switch(scode) {
          case syntax_illegal:
            # illegal -> issue Error:
            pushSTACK(*stream_); # value for slot STREAM of STREAM-ERROR
            pushSTACK(ch); # character
            pushSTACK(*stream_); # Stream
            pushSTACK(S(read));
            fehler(stream_error,
                   GETTEXT("~ from ~: illegal character ~")
                  );
            break;
          case syntax_single_esc:
            # Single-Escape-Character ->
            # read next character and take over unchanged
            escape_flag = true;
            read_char_syntax(ch = ,scode = ,stream_); # read next character
            if (scode==syntax_eof) { # reached EOF?
              pushSTACK(*stream_); # value for slot STREAM of STREAM-ERROR
              pushSTACK(*stream_);
              pushSTACK(S(read));
              fehler(end_of_file,
                     GETTEXT("~: input stream ~ ends within a token after single escape character")
                    );
            }
          escape:
            # past Escape-character:
            # take over character into token without change
            ssstring_push_extend(STACK_1,char_code(ch));
            ssbvector_push_extend(STACK_0,a_escaped);
            break;
          case syntax_multi_esc:
            # Multiple-Escape-character
            multiple_escape_flag = !multiple_escape_flag;
            escape_flag = true;
            break;
          case syntax_constituent:
          case syntax_nt_macro:
            # normal constituent
            if (multiple_escape_flag) # between Multiple-Escape-characters?
              goto escape; # yes -> take over character without change
            # take over into token (capital-conversion takes place later):
            {
              var chart c = char_code(ch);
              ssstring_push_extend(STACK_1,c);
              ssbvector_push_extend(STACK_0,attribute_of(c));
            }
            break;
          case syntax_whitespace:
          case syntax_t_macro:
            # whitespace or terminating macro ->
            # Token ends before this Character.
            if (multiple_escape_flag) # between multiple-escape-characters?
              goto escape; # yes -> take over character without change
            # Token is finished.
            # Push back character to the Stream,
            # if ( it is no Whitespace ) or
            # ( it is a  Whitespace and also  *READ-PRESERVE-WHITESPACE* /= NIL holds true).
            if ((!(scode == syntax_whitespace))
                || test_value(S(read_preserve_whitespace))
               )
              unread_char(stream_,ch);
            goto ende;
          case syntax_eof:
            # EOF erreicht.
            if (multiple_escape_flag) { # between multiple-escape-character?
              pushSTACK(*stream_); # value for slot STREAM of STREAM-ERROR
              pushSTACK(*stream_);
              pushSTACK(S(read));
              fehler(end_of_file,
                     GETTEXT("~: input stream ~ ends within a token after multiple escape character")
                    );
            }
            # no -> token is finished normally
            goto ende;
          default: NOTREACHED
        }
      }
     ende:
      # now token is finished, multiple_escape_flag = false.
      token_escape_flag = escape_flag; # store Escape-Flag
      O(token_buff_2) = popSTACK(); # Attributecode-Buffer
      O(token_buff_1) = popSTACK(); # Character-Buffer
    }

# --------------- READ between token-level and objekt-level ------------------

# UP: checks, if the token-buffer contains a potential-number, and
# transforms Attributecodes as preparation on read-routines for digits.
# test_potential_number_syntax(&base,&token_info);
# > O(token_buff_1): read characters
# > O(token_buff_2): their attributecodes
# > base: base of number-system (value of *READ-BASE* or *PRINT-BASE*)
# < base: base of number-system (= 10 or old base)
# conversion takes place within O(token_buff_2):
#   if potential number:
#     >=a_letter above the base of number-system -> a_alpha
#   if not potential number:
#     distinction between [a_pack_m | a_dot | others] is preserved.
# < result: true, if potential number
#             (and then token_info is filled with {charptr, attrptr, len} )
  typedef struct {
    chart* charptr;
    uintB* attrptr;
    uintL len;
  } token_info;
  local bool test_potential_number_syntax (uintWL* base_, token_info* info);
  local bool test_potential_number_syntax(base_,info)
    var uintWL* base_;
    var token_info* info;
    # A token is a potential number, if (CLTL, p. 341)
    # - it consists exclusively of digits, '+','-','/','^','_','.' and
    #   Number-Markers. The base for the digits ist context-sensitive.
    #   It is always 10, if a dot '.' is in the token.
    #   A Number-Marker is a letter, that is no digit and
    #   is not placed adjacent to another such letter.
    # - it contains at least one digit,
    # - it starts with a digit, '+','-','.','^' or '_',
    # - it does not end with '+' or '-'.
    # Verification:
    # 1. Search for a dot. if ther is one ===> Base:=10.
    # 2. Every char that is >=a_letter (also 'A'-'Z','a'-'z')  and has a value <Basis,
    #    will be converted to an a_digit.
    # (Now a_digit is interpreted as "digit" and >=a_letter as "letter".)
    # 3. Test, if only chars >=a_ratio are in the token. No -> no potential number.
    # 4. Test, if an a_digit is in the token. No -> no potential number.
    # (No the length is >0.)
    # 5. Test, if adjacend >=a_letter are in the token.
    #    Yes -> no potential number.
    # 6. Test, if first character attribute is  >=a_dot and <=a_digit.
    #    No -> no potential number.
    # 7. Test, if last character attribute is =a_plus or =a_minus.
    #    Yes -> no potential number.
    # 8. Otherwise it is a potential number.
    {
      var chart* charptr0; # Pointer to the characters
      var uintB* attrptr0; # Pointer to the attributes
      var uintL len; # Length of token
      # initialize:
      {
        var object buff = O(token_buff_1); # Semi-Simple String
        len = TheIarray(buff)->dims[1]; # length = Fill-Pointer
        charptr0 = &TheSstring(TheIarray(buff)->data)->data[0]; # characters from this point on
        buff = O(token_buff_2); # Semi-Simple Byte-Vektor
        attrptr0 = &TheSbvector(TheIarray(buff)->data)->data[0]; # attributecodes from this point on
      }
      # 1. search, if thereis a dot:
      {
        if (len > 0) {
          var uintB* attrptr = attrptr0;
          var uintL count;
          dotimespL(count,len, {
            if (*attrptr++ == a_dot) goto dot;
          });
        }
        # no dot -> leave base unchanged
        goto no_dot;
        # dot -> base := 10
       dot: *base_ = 10;
       no_dot: ;
      }
      # 2. translate everything  >=a_letter with value <Basis into a_digit:
      if (len > 0) {
        var uintB* attrptr = attrptr0;
        var chart* charptr = charptr0;
        var uintL count;
        dotimespL(count,len, {
          if (*attrptr >= a_letter) {
            # Attributecode >= a_letter
            var cint c = as_cint(*charptr); # character, must be 'A'-'Z','a'-'Z'
            if (c >= 'a') { c -= 'a'-'A'; }
            if ((c - 'A') + 10 < *base_) # value < base ?
              *attrptr = a_digit; # translate into a_digit
          }
          attrptr++; charptr++;
        });
      }
      # 3. Test, if only attributecodes >=a_ratio occur:
      if (len > 0) {
        var uintB* attrptr = attrptr0;
        var uintL count;
        dotimespL(count,len, {
          if (!(*attrptr++ >= a_ratio))
            return false; # no -> no potential number
        });
      }
      # 4. Test, if an a_digit occurs:
      {
        if (len > 0) {
          var uintB* attrptr = attrptr0;
          var uintL count;
          dotimespL(count,len, {
            if (*attrptr++ == a_digit)
              goto digit_ok;
          });
        }
        return false; # no potential number
       digit_ok: ;
      }
      # length len>0.
      # 5. Test, if two attributecodes >= a_letter follow adjacently:
      if (len > 1) {
        var uintB* attrptr = attrptr0;
        var uintL count;
        dotimespL(count,len-1, {
          if (*attrptr++ >= a_letter)
            if (*attrptr >= a_letter)
              return false;
        });
      }
      # 6. Test, if first attributecode is >=a_dot and <=a_digit:
      {
        var uintB attr = attrptr0[0];
        if (!((attr >= a_dot) && (attr <= a_digit)))
          return false;
      }
      # 7. Test, if last attributecode is  =a_plus or =a_minus:
      {
        var uintB attr = attrptr0[len-1];
        if ((attr == a_plus) || (attr == a_minus))
          return false;
      }
      # 8. It is a potential number.
      info->charptr = charptr0; info->attrptr = attrptr0; info->len = len;
      return true;
    }

# UP: verifies, if the token-buffer contains a number (syntax according to CLTL
# Table 22-2), and provides the parameters which are necessary for the translation
# into a number, where necessary.
# test_number_syntax(&base,&string,&info)
# > O(token_buff_1): read characters
# > O(token_buff_2): their attributecodes
# > token_escape_flag: Escape-Character-Flag
# > base: number-system-base (value of *READ-BASE* or *PRINT-BASE*)
# < base: number-system-base
# < string: Normal-Simple-String with the characters
# < info.sign: sign (/=0 if negative)
# < result: number-type
#     0 : no number (then also base,string,info are meaningless)
#     1 : Integer
#         < index1: Index of the first digit
#         < index2: Index after the last digit
#         (that means index2-index1 digits, incl. a possible decimal
#         dot at the end)
#     2 : Rational
#         < index1: Index of the first digit
#         < index3: Index of '/'
#         < index2: Index after the last digit
#         (that means index3-index1 numerator-digits and
#          index2-index3-1 denominator-digits)
#     3 : Float
#         < index1: Index of the start of mantissa (excl. sign)
#         < index4: Index after the end of mantissa
#         < index2: Index at the  end of the characters
#         < index3: Index after the dezimal dot (=index4 if there is no dot)
#         (implies: mantissa with index4-index1 characters: digits and at
#          most one '.')
#         (implies: index4-index3 digits after the dot)
#         (implies: if index4<index2: index4 = Index of the exponent-marker,
#               index4+1 = index of exponenten-sign or of the first
#               exponenten-digit)
  typedef struct {
    signean sign;
    uintL index1;
    uintL index2;
    uintL index3;
    uintL index4;
  } zahl_info;
  local uintWL test_number_syntax (uintWL* base_, object* string_, zahl_info* info);
  local uintWL test_number_syntax(base_,string_,info)
    var uintWL* base_;
    var object* string_;
    var zahl_info* info;
    # Method:
    # 1. test for potential number.
    #    Then there exist only Attributcodes >= a_ratio,
    #    and with a_dot, the base=10.
    # 2. read sign { a_plus | a_minus | } and store.
    # 3. try to read token as a rational number:
    #    test, if syntax
    #    { a_plus | a_minus | }                               # already read
    #    { a_digit < base }+ { a_ratio { a_digit < base }+ | }
    #    is matching.
    # 4. set base:=10, and if base was >10 beforehand, assign the attributcodes to
    #    the Characters 'A'-'Z','a'-'z' (which have been  a_letter oder a_expo_m earlier,
    #    but might have been transformed in a_digit by test_potential_number_syntax)
    #    according to table again (a_letter -> no number or a_expo_m).
    # 5. try to interprete the token as a  floating-point-number or decimal-integer:
    #    Test, if the syntax
    #    { a_plus | a_minus | }                               # already read
    #    { a_digit }* { a_dot { a_digit }* | }
    #    { a_expo_m { a_plus | a_minus | } { a_digit }+ | }
    #    is matching.
    #    if there is an exponent, there must be digits before or after the dot;
    #      it is a float, Type will be determined by exponent-marker
    #      (e,E deliver the value of the variable *read-default-float-format* as type).
    #    if there is no exponent:
    #      if there is no dot, it is not a number (should have been delivered at
    #        step 3, but base obviously did not fit).
    #      if decimal dot exists:
    #        if there are digits after the dot, it is a float (type is
    #          denoted by the variable *read-default-float-format*).
    #        if there are no digits after the dot:
    #          if there were digits before the dot --> decimal-integer.
    #          otherwise no number.
    {
      var chart* charptr0; # Pointer to the characters
      var uintB* attrptr0; # Pointer to the attributes
      var uintL len; # length of the token
      # 1. test for potential number:
      {
        if (token_escape_flag) # token with escape-character ->
          return 0; # no potential number -> no number
         # escape-flag deleted.
        var token_info info;
        if (!test_potential_number_syntax(base_,&info)) # potential number ?
          return 0; # nein -> keine Zahl
        # yes -> read outputparameter returned by test_potential_number_syntax:
        charptr0 = info.charptr;
        attrptr0 = info.attrptr;
        len = info.len;
      }
      *string_ = TheIarray(O(token_buff_1))->data; # Normal-Simple-String
      var uintL index0 = 0;
      # read 2. sign and store:
      info->sign = 0; # sign:=positiv
      switch (*attrptr0) {
        case a_minus: info->sign = -1; # sign:=negativ
        case a_plus:
          # read over sign:
          charptr0++; attrptr0++; index0++;
        default:
          break;
      }
      info->index1 = index0; # Startindex
      info->index2 = len; # Endindex
      # info->sign, info->index1 and info->index2 finished.
      # charptr0 and attrptr0 and index0 from now on unchanged.
      var uintB flags = 0; # delete all flags
      # 3. Rational number
      {
        var chart* charptr = charptr0;
        var uintB* attrptr = attrptr0;
        var uintL index = index0;
        # flags & bit(0)  indicates, if an a_digit < base
        #                 has already arrived.
        # flags & bit(1)  indicates, if an a_ratio has already arrived
        #                 (and then info->index3 is its position)
        loop {
          # next character
          if (index>=len)
            break;
          var uintB attr = *attrptr++; # its attributcode
          if (attr==a_digit) {
            var cint c = as_cint(*charptr++); # character (Digit, namely '0'-'9','A'-'Z','a'-'z')
            # determine value:
            var uintB wert = (c<'A' ? c-'0' : c<'a' ? c-'A'+10 : c-'a'+10);
            if (wert >= *base_) # Digit with value >=base ?
              goto schritt4; # yes -> no rational number
            # Digit with value <base
            flags |= bit(0); # set bit 0
            index++;
          } elif (attr==a_ratio) {
            if (flags & bit(1)) # not the only '/' ?
              goto schritt4; # yes -> not a rational number
            flags |= bit(1); # first '/'
            if (!(flags & bit(0))) # no digits before the fraction bar?
              goto schritt4; # yes -> not a rational number
            flags &= ~bit(0); # delete bit 0, new block starts
            info->index3 = index; # store index of '/'
            charptr++; index++;
          } else
            # Attributecode /= a_digit, a_ratio -> not a rational number
            goto schritt4;
        }
        # Token finished
        if (!(flags & bit(0))) # no digits in the last block ?
          goto schritt4; # yes -> not a rational number
        # rational number
        if (!(flags & bit(1))) # a_ratio?
          # no -> it's an integer, info is ready.
          return 1;
        else
          # yes -> it's a fraction, info is ready.
          return 2;
      }
     schritt4:
      # 4. base:=10, with elimination of 'A'-'Z','a'-'z'
      if (*base_ > 10) {
        var uintL count = len-index0;
        if (count > 0) {
          var chart* charptr = charptr0;
          var uintB* attrptr = attrptr0;
          dotimespL(count,count, {
            var chart ch = *charptr++; # next character
            var cint c = as_cint(ch);
            if (((c>='A') && (c<='Z')) || ((c>='a') && (c<='z'))) {
              var uintB attr = attribute_of(ch); # its true Attributcode
              if (attr == a_letter) # is er = a_letter ?
                return 0; # yes -> not a number
              # otherwise write (must be a_expo_m):
              *attrptr = attr;
            }
            attrptr++;
          });
        }
      }
      *base_ = 10;
      # 5. Floating-Point-Number or decimal-integer
      {
        var uintB* attrptr = attrptr0;
        var uintL index = index0;
        # flags & bit(2)  indicates, if an a_dot has arrived already
        #                 (then info->index3 is the subsequent position)
        # flags & bit(3)  zeigt an, ob im letzten Ziffernblock bereits ein
        #                 a_digit angetroffen wurde.
        # flags & bit(4)  indicates, if there was an a_dot with digits in front
        #                 of it
        loop {
          # next character
          if (index>=len)
            break;
          var uintB attr = *attrptr++; # its attributecode
          if (attr==a_digit) {
            # Digit ('0'-'9')
            flags |= bit(3); index++;
          } elif (attr==a_dot) {
            if (flags & bit(2)) # not the only '.' ?
              return 0; # yes -> not a number
            flags |= bit(2); # first '.'
            if (flags & bit(3))
              flags |= bit(4); # maybe with digits in front of the dot
            flags &= ~bit(3); # reset flag
            index++;
            info->index3 = index; # store index after the '.'
          } elif (attr==a_expo_m)
            goto expo; # treat exponent
          else
            return 0; # not a float, thus not a number
        }
        # token finished, no exponent
        if (!(flags & bit(2))) # only decimal digits without '.' ?
          return 0; # yes -> not a number
        info->index4 = index;
        if (flags & bit(3)) # with digits behind the dot?
          return 3; # yes -> Float, info ready.
        # no.
        if (!(flags & bit(4))) # also without digits in front of dot?
          return 0; # yes -> only '.' -> no number
        # only digits in front of '.',none behind it -> decimal-integer.
        # Don't need to cut '.' away at the end (will be omitted).
        return 1;
       expo:
        # reached exponent.
        info->index4 = index;
        index++; # count exponent-marker
        if (!(flags & bit(2)))
          info->index3 = info->index4; # default for index3
        if (!(flags & (bit(3)|bit(4)))) # were there digits in front of
                                        # or behind the dot?
          return 0; # no -> not a number
        # continue with exponent:
        # flags & bit(5)  indicates, if there has already been
        # an exponent-digit.
        if (index>=len)
          return 0; # string finished -> not a number
        switch (*attrptr) {
          case a_plus:
          case a_minus:
            attrptr++; index++; # skip sign of the exponent
          default:
            break;
        }
        loop {
          # next character in exponent:
          if (index>=len)
            break;
          # from now on only digits are allowed:
          if (!(*attrptr++ == a_digit))
            return 0;
          flags |= bit(5);
          index++;
        }
        # Token is finished after exponent
        if (!(flags & bit(5))) # no digit in exponent?
          return 0; # yes -> not a number
        return 3; # Float, info ready.
      }
    }

# Handler: Signals a READER-ERROR with the same error message as the current
# condition.
  local void signal_reader_error (void* sp, object* frame, object label, object condition);
  local void signal_reader_error(sp,frame,label,condition)
    var void* sp;
    var object* frame;
    var object label;
    var object condition;
    {
      # (SYS::ERROR-OF-TYPE 'READER-ERROR "~A" condition)
      pushSTACK(S(reader_error)); pushSTACK(O(tildeA)); pushSTACK(condition);
      funcall(L(error_of_type),3);
    }

# UP: checks, if a token consists only of Dots.
# test_dots()
# > O(token_buff_1): read characters
# > O(token_buff_2): their attributcodes
# < result: true, if token is empty or consists only of dots
  local bool test_dots (void);
  local bool test_dots()
    {
      # search for attributecode /= a_dot:
      var object bvec = O(token_buff_2); # Semi-Simple-Byte-Vector
      var uintL len = TheIarray(bvec)->dims[1]; # Fill-Pointer
      if (len > 0) {
        var uintB* attrptr = &TheSbvector(TheIarray(bvec)->data)->data[0];
        var uintL count;
        dotimespL(count,len, {
          if (!(*attrptr++ == a_dot)) # Attributcode /= a_dot found?
            return false; # yes -> ready, false
        });
      }
      # only dots.
      return true;
    }

# UP: converts a number-token into capitals.
# upcase_token();
# > O(token_buff_1): read characters
# > O(token_buff_2): their attributecodes
  local void upcase_token (void);
  local void upcase_token()
    {
      var object string = O(token_buff_1); # Semi-Simple-String
      var uintL len = TheIarray(string)->dims[1]; # Fill-Pointer
      if (len > 0) {
        var chart* charptr = &TheSstring(TheIarray(string)->data)->data[0];
        dotimespL(len,len, { *charptr = up_case(*charptr); charptr++; } );
      }
    }

# UP: converts a piece of the read Tokens into upper or lower case letters.
# case_convert_token(start_index,end_index,direction);
# > O(token_buff_1): read characters
# > O(token_buff_2): their attributecodes
# > uintL start_index: startindex of range to be converted
# > uintL end_index: endindex of the range to be converted
# > uintW direction: direction of the conversion
  local void case_convert_token (uintL start_index, uintL end_index, uintW direction);
  local void case_convert_token(start_index,end_index,direction)
    var uintL start_index;
    var uintL end_index;
    var uintW direction;
    {
      var chart* charptr = &TheSstring(TheIarray(O(token_buff_1))->data)->data[start_index];
      var uintB* attrptr = &TheSbvector(TheIarray(O(token_buff_2))->data)->data[start_index];
      var uintL len = end_index - start_index;
      if (len == 0)
        return;
      switch (direction) {
        case case_upcase:
          # convert un-escaped characters to upper case:
        do_upcase:
          dotimespL(len,len, {
            if (!(*attrptr == a_escaped))
              *charptr = up_case(*charptr);
            charptr++; attrptr++;
          });
          break;
        case case_downcase:
          # convert un-escaped characters to lower case:
        do_downcase:
          dotimespL(len,len, {
            if (!(*attrptr == a_escaped))
              *charptr = down_case(*charptr);
            charptr++; attrptr++;
          });
          break;
        case case_preserve:
          # do nothing.
          break;
        case case_invert:
          # if there is no un-escaped lower-case-letter,
          # convert all un-escaped characters to lower case.
          # if there is no un-escaped upper-case-letter,
          # convert all un-escaped characters to upper case.
          # otherwise do nothing.
          {
            var bool seen_uppercase = false;
            var bool seen_lowercase = false;
            var const chart* cptr = charptr;
            var const uintB* aptr = attrptr;
            var uintL count;
            dotimespL(count,len, {
              if (!(*aptr == a_escaped)) {
                var chart c = *cptr;
                if (!chareq(c,up_case(c)))
                  seen_lowercase = true;
                if (!chareq(c,down_case(c)))
                  seen_uppercase = true;
              }
              cptr++; aptr++;
            });
            if (seen_uppercase) {
              if (!seen_lowercase)
                goto do_downcase;
            } else {
              if (seen_lowercase)
                goto do_upcase;
            }
          }
          break;
        default: NOTREACHED
      }
    }

# UP: converts the whole read token to upper or lower case.
# case_convert_token_1();
  local void case_convert_token_1 (void);
  local void case_convert_token_1()
    {
      var object readtable;
      get_readtable(readtable = );
      var uintW direction = (uintW)posfixnum_to_L(TheReadtable(readtable)->readtable_case);
      var uintL len = TheIarray(O(token_buff_1))->dims[1]; # Length = Fill-Pointer
      case_convert_token(0,len,direction);
    }

# UP: treatment of read-macro-character:
# calls the appropriate macro-function; for dispatch-characters read
# number-argument and subchar first.
# read_macro(ch,&stream)
# > ch: macro-character, a character
# > stream: Stream
# < stream: Stream
# < mv_count/mv_space: one value at most
# can trigger GC
  local Values read_macro (object ch, const object* stream_);
  local Values read_macro(ch,stream_)
    var object ch;
    var const object* stream_;
    {
      var object readtable;
      get_readtable(readtable = ); # current readtable (don't need to save ch)
      var object macrodef = # fetch macro-definition from table
        perchar_table_get(TheReadtable(readtable)->readtable_macro_table,char_code(ch));
      if (nullp(macrodef)) { # =NIL ?
        pushSTACK(*stream_); # value for slot STREAM of STREAM-ERROR
        pushSTACK(ch);
        pushSTACK(*stream_);
        pushSTACK(S(read));
        fehler(stream_error,
               GETTEXT("~ from ~: ~ has no macro character definition")
              );
      }
      if (!simple_vector_p(macrodef)) { # a simple-vector?
        # ch normal macro-character, macrodef function
        pushSTACK(*stream_); # stream as 1st argument
        pushSTACK(ch); # character as 2nd argument
        funcall(macrodef,2); # call function
        if (mv_count > 1) {
          pushSTACK(fixnum(mv_count)); # value number as Fixnum
          pushSTACK(ch);
          pushSTACK(*stream_);
          pushSTACK(S(read));
          fehler(error,
                 GETTEXT("~ from ~: macro character definition for ~ may not return ~ values, only one value.")
                );
        }
        # at most one value.
        return; # retain mv_space/mv_count
      } else {
        # Dispatch-Macro-Character.
        # When this changes, keep DISPATCH-READER in defs2.lisp up to date.
        pushSTACK(macrodef); # save vector
        var object arg; # argument (Integer >=0 or NIL)
        var object subch; # sub-char
        var chart subc; # sub-char
        # read digits of argument:
        {
          var bool flag = false; # flag, if there has been a digit already
          pushSTACK(Fixnum_0); # previous Integer := 0
          loop {
            var object nextch = read_char(stream_); # read character
            if (eq(nextch,eof_value)) {
              pushSTACK(*stream_); # value for slot STREAM of STREAM-ERROR
              pushSTACK(ch); # main char
              pushSTACK(*stream_); # Stream
              pushSTACK(S(read));
              fehler(end_of_file,
                     GETTEXT("~: input stream ~ ends within read macro beginning to ~")
                    );
            }
            # otherwise check for character.
            if (!charp(nextch))
              fehler_charread(nextch,stream_);
            var chart ch = char_code(nextch);
            var cint c = as_cint(ch);
            if (!((c>='0') && (c<='9'))) { # no digit -> loop finished
              subc = ch;
              break;
            }
            # multiply Integer by 10 and add digit:
            STACK_0 = mal_10_plus_x(STACK_0,(c-'0'));
            flag = true;
          }
          # argument in STACK_0 finished (only if flag=true).
          arg = popSTACK();
          if (!flag)
            arg = NIL; # there was no digit -> Argument := NIL
        }
        # let's continue with Subchar (Character subc)
        subch = code_char(subc);
        subc = up_case(subc); # convert Subchar to upper case
        macrodef = popSTACK(); # get back Vector
        macrodef = perchar_table_get(macrodef,subc); # Subchar-Function or NIL
        if (nullp(macrodef)) {
          # NIL -> undefined
          pushSTACK(*stream_); # Wert for slot STREAM of STREAM-ERROR
          pushSTACK(subch); # Subchar
          pushSTACK(ch); # Mainchar
          pushSTACK(*stream_); # Stream
          pushSTACK(S(read));
          fehler(stream_error,
                 GETTEXT("~ from ~: After ~ is ~ an undefined dispatch macro character")
                );
        }
        pushSTACK(*stream_); # Stream as 1. argument
        pushSTACK(subch); # Subchar as 2. Argument
        pushSTACK(arg); # Argument (NIL or Integer>=0) as 3. Argument
        funcall(macrodef,3); # call function
        if (mv_count > 1) {
          pushSTACK(fixnum(mv_count)); # value number as Fixnum
          pushSTACK(ch); # Mainchar
          pushSTACK(subch); # Subchar
          pushSTACK(*stream_); # Stream
          pushSTACK(S(read));
          fehler(error,
                 GETTEXT("~ from ~: dispatch macro character definition for ~ after ~ may not return ~ values, only one value.")
                );
        }
        # at most 1 value.
        return; # retain mv_space/mv_count
      }
    }

# ------------------------ READ at object-level ------------------------------

# UP: reads an object.
# skip leading  whitespace and comment.
# the curren values of SYS::*READ-PRESERVE-WHITESPACE* are definitive
# (for potentially skipping the first Whitespace behind the object)
# also devinitive is SYS::*READ-RECURSIVE-P* (for EOF-treatment).
# read_internal(&stream)
# > stream: Stream
# < stream: Stream
# < result: read object (eof_value at EOF, dot_value for single dot)
# can trigger GC
  local object read_internal (const object* stream_);
  local object read_internal(stream_)
    var const object* stream_;
    {
     wloop: # loop for skipping of leading whitespace/comment:
      {
        var object ch;
        var uintWL scode;
        read_char_syntax(ch = ,scode = ,stream_); # read character
        switch(scode) {
          case syntax_whitespace:
            # Whitespace -> throw away and continue reading
            goto wloop;
          case syntax_t_macro:
          case syntax_nt_macro:
            # Macro-Character at start of Token
            read_macro(ch,stream_); # call Macro-Function
            if (mv_count==0)
              # 0 values -> continue reading
              goto wloop;
            else
              # 1 value -> as result
              return value1;
          case syntax_eof:
            # EOF at start of Token
            if (test_value(S(read_recursive_p))) # *READ-RECURSIVE-P* /= NIL ?
              # yes -> EOF within an object -> error
              fehler_eof_innen(stream_);
            # otherwise eof_value as value:
            return eof_value;
          case syntax_illegal:
            # read_token_1 returns Error
          case syntax_single_esc:
          case syntax_multi_esc:
          case syntax_constituent:
            # read Token: A Token starts with character ch.
            read_token_1(stream_,ch,scode); # finish reading of Token
            break;
          default: NOTREACHED
        }
      }
      # reading of Token finished
      if (test_value(S(read_suppress))) # *READ-SUPPRESS* /= NIL ?
        return NIL; # yes -> don't interprete Token, NIL as value
      # Token must be interpreted
      # the Token is in O(token_buff_1), O(token_buff_2), token_escape_flag.
      if ((!token_escape_flag) && test_dots()) {
        # Token is a sequence of Dots, read without escape-characters
        # thus Length is automatically >0.
        var uintL len = TheIarray(O(token_buff_1))->dims[1]; # length of Token
        if (len > 1) {
          # Length>1 -> error
          pushSTACK(*stream_); # value for slot STREAM of STREAM-ERROR
          pushSTACK(*stream_);
          pushSTACK(S(read));
          fehler(stream_error,
                 GETTEXT("~ from ~: a token consisting only of dots cannot be meaningfully read in")
                );
        }
        # Length=1 -> dot_value as value
        return dot_value;
      }
      # Token is OK
      {
        var uintWL base = get_read_base(); # value of *READ-BASE*
        # Token can be interpreted as number?
        var object string;
        var zahl_info info;
        var uintWL numtype = test_number_syntax(&base,&string,&info);
        if (!(numtype==0)) { # number?
          upcase_token(); # convert to upper case
          var object result;
          # ANSI CL 2.3.1.1 requires that we transform ARITHMETIC-ERROR into READER-ERROR
          make_HANDLER_frame(O(handler_for_arithmetic_error),&signal_reader_error,NULL);
          switch (numtype) {
            case 1: # Integer
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
        }
      }
      # Token cannot be interpreted as number.
      # we interprete the Token as Symbol (even, if the Token matches
      # Potential-number-Syntax, thus being a 'reserved token' (in the spirit
      # of CLTL S. 341 top) ).
      # first determine the distribution of colons (Characters with
      # Attributecode a_pack_m):
      # Beginning at the front, search the first colon. Cases (CLTL S. 343-344):
      # 1. no colon -> current Package
      # 2. one or two colons at the beginning -> Keyword
      # 3. one colon, not at the beginning -> external Symbol
      # 4. two Doppelpunkte, not at the beginning -> internal Symbol
      # In the last three cases no more colons may occur.
      # (It cannot be checked here , that at step 2. the name-part respectively at
      # 3. and 4. der Package-part and the Name-part don't have the syntax of a number,
      # because TOKEN_ESCAPE_FLAG is valid for the whole Token.
      # Compare |USER|:: and |USER|::|| )
      {
        var uintW direction; # direction of the case-conversion
        {
          var object readtable;
          get_readtable(readtable = );
          direction = (uintW)posfixnum_to_L(TheReadtable(readtable)->readtable_case);
        }
        var object buff_2 = O(token_buff_2); # Attributecode-Buffer
        var uintL len = TheIarray(buff_2)->dims[1]; # length = Fill-Pointer
        var uintB* attrptr = &TheSbvector(TheIarray(buff_2)->data)->data[0];
        var uintL index = 0;
        # always attrptr = &TheSbvector(...)->data[index].
        # Token is split in Packagename and Name:
        var uintL pack_end_index;
        var uintL name_start_index;
        var bool external_internal_flag = false; # preliminary external
        loop {
          if (index>=len)
            goto current; # found no colon -> current package
          if (*attrptr++ == a_pack_m)
            break;
          index++;
        }
        # found first colon at Index index
        pack_end_index = index; # Packagename ends here
        index++;
        name_start_index = index; # Symbolname starts (preliminary) here
        # reached Tokenend -> external Symbol:
        if (index>=len)
          goto ex_in_ternal;
        # is a further colon following, immediately?
        index++;
        if (*attrptr++ == a_pack_m) {
          # two colons side by side
          name_start_index = index; # Symbolname is starting but now
          external_internal_flag = true; # internal
        } else {
          # first colon was isolated
          # external
        }
        # no more colons are to come:
        loop {
          if (index>=len)
            goto ex_in_ternal; # no further colon found -> ok
          if (*attrptr++ == a_pack_m)
            break;
          index++;
        }
        # error message
        pushSTACK(*stream_); # Wert for slot STREAM of STREAM-ERROR
        pushSTACK(copy_string(O(token_buff_1))); # copy Character-Buffer
        pushSTACK(*stream_); # Stream
        pushSTACK(S(read));
        fehler(stream_error,
               GETTEXT("~ from ~: too many colons in token ~")
              );
        # search Symbol or create it:
       current: # search Symbol in the current package.
        # Symbolname = O(token_buff_1) = (subseq O(token_buff_1) 0 len)
        # is a non-simple String.
        {
          var object pack = get_current_package();
          if (!pack_casesensitivep(pack))
            case_convert_token(0,len,direction);
          # intern Symbol  (and copy String, if the Symbol
          # must be created freshly):
          var object sym;
          intern(O(token_buff_1),pack,&sym);
          return sym;
        }
       ex_in_ternal: # build external/internal Symbol
        # Packagename = (subseq O(token_buff_1) 0 pack_end_index),
        # Symbolname = (subseq O(token_buff_1) name_start_index len).
        case_convert_token(0,pack_end_index,direction);
        if (pack_end_index==0) {
          # colon(s) at the beginning -> build Keyword:
          # Symbolname = (subseq O(token_buff_1) name_start_index len).
          case_convert_token(name_start_index,len,direction);
          # adjust auxiliary-String:
          var object hstring = O(displaced_string);
          TheIarray(hstring)->data = O(token_buff_1); # Data-vector
          TheIarray(hstring)->dims[0] = name_start_index; # Displaced-Offset
          TheIarray(hstring)->totalsize =
            TheIarray(hstring)->dims[1] = len - name_start_index; # length
          # intern Symbol in the Keyword-Package  (and copy String,
          # if the Symbol must be created newly):
          return intern_keyword(hstring);
        }
        {
          # Packagename = (subseq O(token_buff_1) 0 pack_end_index).
          # adjust Auxiliary-String:
          var object hstring = O(displaced_string);
          TheIarray(hstring)->data = O(token_buff_1); # Data-vector
          TheIarray(hstring)->dims[0] = 0; # Displaced-Offset
          TheIarray(hstring)->totalsize =
            TheIarray(hstring)->dims[1] = pack_end_index; # length
          # search Package with this name:
          var object pack = find_package(hstring);
          if (nullp(pack)) { # Package not found?
            pushSTACK(copy_string(hstring)); # copy Displaced-String, Wert for Slot PACKAGE of PACKAGE-ERROR
            pushSTACK(STACK_0);
            pushSTACK(*stream_); # Stream
            pushSTACK(S(read));
            fehler(package_error,
                   GETTEXT("~ from ~: there is no package with name ~")
                  );
          }
          if (!pack_casesensitivep(pack))
            case_convert_token(name_start_index,len,direction);
          # adjust Auxiliary-String:
          TheIarray(hstring)->dims[0] = name_start_index; # Displaced-Offset
          TheIarray(hstring)->totalsize =
            TheIarray(hstring)->dims[1] = len - name_start_index; # Length
          if (external_internal_flag) {
            # internal
            # intern Symbol (and copy String,
            # if  Symbol must be created newly):
            var object sym;
            intern(hstring,pack,&sym);
            return sym;
          } else {
            # external
            # search external Symbol with this Printnamen:
            var object sym;
            if (find_external_symbol(hstring,pack,&sym)) {
              return sym; # found sym
            } else {
              pushSTACK(pack); # value for Slot PACKAGE of PACKAGE-ERROR
              pushSTACK(copy_string(hstring)); # copy Displaced-String
              pushSTACK(STACK_1); # pack
              pushSTACK(*stream_); # Stream
              pushSTACK(S(read));
              fehler(package_error,
                     GETTEXT("~ from ~: ~ has no external symbol with name ~")
                    );
            }
          }
        }
      }
    }

# UP: reads an Objekt, with SYS::*READ-RECURSIVE-P* /= NIL
# (and SYS::*READ-PRESERVE-WHITESPACE* = NIL, cmp. CLTL p. 377 middle).
# reports error at EOF.
# read_recursive(&stream)
# > stream: Stream
# < stream: Stream
# < result: read Objekt (dot_value at single dot)
# can trigger GC
  local object read_recursive (const object* stream_);
  local object read_recursive(stream_)
    var const object* stream_;
    {
      check_SP(); check_STACK(); # check Stacks for Overflow
      if (test_value(S(read_recursive_p))) {
        #  recursive
        return read_internal(stream_);
      } else {
        # bind SYS::*READ-RECURSIVE-P* to T:
        dynamic_bind(S(read_recursive_p),T);
        # and bind SYS::*READ-PRESERVE-WHITESPACE* to NIL:
        dynamic_bind(S(read_preserve_whitespace),NIL);
        # and read Objekt:
        var object ergebnis = read_internal(stream_);
        dynamic_unbind();
        dynamic_unbind();
        return ergebnis;
      }
    }

# error-message because of out-of-place Dot
# fehler_dot(stream); english: error_dot(stream);
# > stream: Stream
  nonreturning_function(local, fehler_dot, (object stream));
  local void fehler_dot(stream)
    var object stream;
    {
      pushSTACK(stream); # Wert for Slot STREAM of STREAM-ERROR
      pushSTACK(stream); # Stream
      pushSTACK(S(read));
      fehler(stream_error,
             GETTEXT("~ from ~: token \".\" not allowed here")
            );
    }

# UP: reads an Object, with SYS::*READ-RECURSIVE-P* /= NIL
# (and SYS::*READ-PRESERVE-WHITESPACE* = NIL, cmp. CLTL p. 377 middle).
# reports Error at EOF or Token ".".
# (this complies with the Idiom (read stream t nil t).)
# read_recursive_no_dot(&stream)
# > stream: Stream
# < stream: Stream
# < result: read Objekt
# can trigger GC
  local object read_recursive_no_dot (const object* stream_);
  local object read_recursive_no_dot(stream_)
    var const object* stream_;
    {
      # call READ rekursively:
      var object ergebnis = read_recursive(stream_);
      # and report Error at ".":
      if (eq(ergebnis,dot_value))
        fehler_dot(*stream_);
      return ergebnis;
    }

# UP: disentangles #n# - References to #n= - markings in an Object.
# > value of SYS::*READ-REFERENCE-TABLE*:
#     Aliste of Pairs (marking . marked Object), where
#     each margink is an Object  #<READ-LABEL n>.
# > obj: Object
# < result: destructively  modified Object without References
  local object make_references (object obj);
  local object make_references(obj)
    var object obj;
    {
      var object alist = Symbol_value(S(read_reference_table));
      # SYS::*READ-REFERENCE-TABLE* = NIL -> nothing to do:
      if (nullp(alist)) {
        return obj;
      } else {
        # check, if SYS::*READ-REFERENCE-TABLE* is an Aliste:
        {
          var object alistr = alist; # run through list
          while (consp(alistr)) {
            # each Listenelement must be a Cons:
            if (!mconsp(Car(alistr)))
              goto fehler_badtable;
            alistr = Cdr(alistr);
          }
          if (!nullp(alistr)) {
           fehler_badtable:
            pushSTACK(S(read_reference_table));
            pushSTACK(S(read));
            fehler(error,
                   GETTEXT("~: the value of ~ has been arbitrarily altered")
                  );
          }
        }
        # Alist alist is OK
        pushSTACK(obj);
        var object bad_reference =
          subst_circ(&STACK_0,alist); # substitute References by Objects
        if (!eq(bad_reference,nullobj)) {
          pushSTACK(unbound); # "value" for Slot STREAM of STREAM-ERROR
          pushSTACK(Symbol_value(S(read_reference_table)));
          pushSTACK(S(read_reference_table));
          pushSTACK(obj);
          pushSTACK(bad_reference);
          pushSTACK(S(read));
          fehler(stream_error,
                 GETTEXT("~: no entry for ~ from ~ in ~ = ~")
                );
        }
        return popSTACK();
      }
    }

# UP: Reads an Object, with SYS::*READ-RECURSIVE-P* = NIL .
# (Top-Level-Call of Reader)
# read_top(&stream,whitespace-p)
# > whitespace-p: indicates, if whitespace has to be consumend afterwards
# > stream: Stream
# < stream: Stream
# < result: read Object (eof_value at EOF, dot_value at single dot)
# can trigger GC
  local object read_top (const object* stream_, object whitespace_p);
  local object read_top(stream_,whitespace_p)
    var const object* stream_;
    var object whitespace_p;
    {
     #if STACKCHECKR
      var object* STACKbefore = STACK; # retain STACK for later
     #endif
      # bind SYS::*READ-RECURSIVE-P* to NIL:
      dynamic_bind(S(read_recursive_p),NIL);
      # and bind SYS::*READ-PRESERVE-WHITESPACE* to whitespace_p:
      dynamic_bind(S(read_preserve_whitespace),whitespace_p);
      # bind SYS::*READ-REFERENCE-TABLE* to the empty Table NIL:
      dynamic_bind(S(read_reference_table),NIL);
      # bind SYS::*BACKQUOTE-LEVEL* to NIL:
      dynamic_bind(S(backquote_level),NIL);
      # read Object:
      var object obj = read_internal(stream_);
      # disentangle references:
      obj = make_references(obj);
      dynamic_unbind();
      dynamic_unbind();
      dynamic_unbind();
      dynamic_unbind();
     #if STACKCHECKR
      # verify, if Stack is cleaned up:
      if (!(STACK == STACKbefore))
        abort(); # if not --> go to Debugger
     #endif
      return obj;
    }

# UP: reads an Object.
# stream_read(&stream,recursive-p,whitespace-p)
# > recursive-p: indicates, if rekursive call of READ, with Error at EOF
# > whitespace-p: indicates, if whitespace has to be consumed afterwards
# > stream: Stream
# < stream: Stream
# < result: read Object (eof_value at EOF, dot_value at single dot)
# can trigger GC
  global object stream_read (const object* stream_, object recursive_p, object whitespace_p);
  global object stream_read(stream_,recursive_p,whitespace_p)
    var const object* stream_;
    var object recursive_p;
    var object whitespace_p;
    {
      if (nullp(recursive_p)) # inquire recursive-p
        # no -> Top-Level-Call
        return read_top(stream_,whitespace_p);
      else
        # ja -> rekursive Call
        return read_recursive(stream_);
    }

# ----------------------------- READ-Macros -----------------------------------

# UP: Read List.
# read_delimited_list(&stream,endch,ifdotted)
# > endch: expected character at the End, a Character
# > ifdotted: #DOT_VALUE if Dotted List is allowed, #EOF_VALUE otherwise
# > stream: Stream
# < stream: Stream
# < result: read Object
# can trigger GC
  local object read_delimited_list (const object* stream_, object endch, object ifdotted);
# Dito with set SYS::*READ-RECURSIVE-P* :
  local object read_delimited_list_recursive (const object* stream_, object endch, object ifdotted);
# first the general function:
  #ifdef RISCOS_CCBUG
    #pragma -z0
  #endif
  local object read_delimited_list(stream_,endch,ifdotted)
    var const object* stream_;
    var object endch;
    var object ifdotted;
    {
      # bind SYS::*READ-LINE-NUMBER* to (SYS::LINE-NUMBER stream)
      # (for error-message, in order to know about the line with the opening parenthese):
      var object lineno = stream_line_number(*stream_);
      dynamic_bind(S(read_line_number),lineno);
      var object ergebnis;
      # possibly bind SYS::*READ-RECURSIVE-P* to T, first:
      if (test_value(S(read_recursive_p))) { # recursive?
        ergebnis = read_delimited_list_recursive(stream_,endch,ifdotted);
      } else {
        # no -> bind SYS::*READ-RECURSIVE-P* to T:
        dynamic_bind(S(read_recursive_p),T);
        ergebnis = read_delimited_list_recursive(stream_,endch,ifdotted);
        dynamic_unbind();
      }
      dynamic_unbind();
      return ergebnis;
    }
  #ifdef RISCOS_CCBUG
    #pragma -z1
  #endif
# then the more special Function:
  local object read_delimited_list_recursive(stream_,endch,ifdotted)
    var const object* stream_;
    var object endch;
    var object ifdotted;
    {
      # don't need to save endch and ifdotted.
      {
        var object object1; # first List element
        loop { # loop, in order to read first Listenelement
          # next non-whitespace Character:
          var object ch;
          var uintWL scode;
          wpeek_char_syntax(ch = ,scode = ,stream_);
          if (eq(ch,endch)) { # is it the expected ending character?
            # yes -> empty List as result
            read_char(stream_); # consume ending character
            return NIL;
          }
          if (scode < syntax_t_macro) { # Macro-Character?
            # no -> read 1. Objekt:
            object1 = read_recursive_no_dot(stream_);
            break;
          } else {
            # yes -> read belonging character and execute Macro-Function:
            ch = read_char(stream_);
            read_macro(ch,stream_);
            if (!(mv_count==0)) { # value back?
              object1 = value1; # yes -> take as 1. Object
              break;
            }
            # no -> skip
          }
        }
        # object1 is the 1. Object
        pushSTACK(object1);
      }
      {
        var object new_cons = allocate_cons(); # tinker start of the List
        Car(new_cons) = popSTACK(); # new_cons = (cons object1 nil)
        pushSTACK(new_cons);
        pushSTACK(new_cons);
      }
      # stack layout: entire_list, (last entire_list).
      loop { # loop over further List elements
        var object object1; # further List element
        loop { # loop in order to read another  List element
          # next non-whitespace Character:
          var object ch;
          var uintWL scode;
          wpeek_char_syntax(ch = ,scode = ,stream_);
          if (eq(ch,endch)) { # Is it the expected Ending character?
            # yes -> finish list
           finish_list:
            read_char(stream_); # consume Ending character
            skipSTACK(1); return popSTACK(); # entire list as result
          }
          if (scode < syntax_t_macro) { # Macro-Character?
            # no -> read next Object:
            object1 = read_recursive(stream_);
            if (eq(object1,dot_value))
              goto dot;
            break;
          } else {
            # yes -> read belonging character and execute Macro-Function:
            ch = read_char(stream_);
            read_macro(ch,stream_);
            if (!(mv_count==0)) { # value back?
              object1 = value1; # yes -> take as next Object
              break;
            }
            # no -> skip
          }
        }
        # insert next Objekt into List:
        pushSTACK(object1);
        {
          var object new_cons = allocate_cons(); # next List-Cons
          Car(new_cons) = popSTACK(); # (cons object1 nil)
          Cdr(STACK_0) = new_cons; # =: (cdr (last Gesamtliste))
          STACK_0 = new_cons;
        }
      }
     dot: # Dot has been read
      if (!eq(ifdotted,dot_value)) # none was allowed?
        fehler_dot(*stream_);
      {
        var object object1; # last List-element
        loop { # loop, in order to read last List-element
          # next non-whitespace Character:
          var object ch;
          var uintWL scode;
          wpeek_char_syntax(ch = ,scode = ,stream_);
          if (eq(ch,endch)) { # is it the expected ending-character?
            # yes -> error
           fehler_dot:
            pushSTACK(*stream_); # value for Slot STREAM of STREAM-ERROR
            pushSTACK(*stream_); # Stream
            pushSTACK(S(read_delimited_list));
            fehler(stream_error,
                   GETTEXT("~ from ~: illegal end of dotted list")
                  );
          }
          if (scode < syntax_t_macro) { # Macro-Character?
            # no -> read last Objekt:
            object1 = read_recursive_no_dot(stream_);
            break;
          } else {
            # yes -> read belonging character and execute Macro-Function:
            ch = read_char(stream_);
            read_macro(ch,stream_);
            if (!(mv_count==0)) { # value back?
              object1 = value1; # yes -> take as last Object
              break;
            }
            # no -> skip
          }
        }
        # object1 is the last Object
        # insert into list as (cdr (last Gesamtliste)):
        Cdr(STACK_0) = object1;
      }
      loop { # loop, in order to read comment after the last List-element
        # next non-whitespace Character:
        var object ch;
        var uintWL scode;
        wpeek_char_syntax(ch = ,scode = ,stream_);
        if (eq(ch,endch)) # Is it the expected Ending-character?
          goto finish_list; # yes -> List finished
        if (scode < syntax_t_macro) # Macro-Character?
          # no -> Dot was there too early, error
          goto fehler_dot;
        else {
          # yes -> read belonging character and execute Macro-Funktion:
          ch = read_char(stream_);
          read_macro(ch,stream_);
          if (!(mv_count==0)) # value back?
            goto fehler_dot; # yes -> Dot came to early, error
          # no -> skip
        }
      }
    }

# Macro: checks the Stream-Argument of a SUBRs.
# stream_ = test_stream_arg(stream);
# > stream: Stream-Argument in STACK
# > subr_self: Caller (a SUBR)
# < stream_: &stream
  #define test_stream_arg(stream)  \
    (!streamp(stream) ? (fehler_stream(stream), (object*)NULL) : &(stream))

# (set-macro-character #\(
#   #'(lambda (stream char)
#       (read-delimited-list #\) stream t :dot-allowed t)
# )   )
LISPFUNN(lpar_reader,2) # reads (
  {
    var object* stream_ = test_stream_arg(STACK_1);
    # read List after '(' until ')', Dot allowed:
    value1 = read_delimited_list(stream_,ascii_char(')'),dot_value); mv_count=1;
    skipSTACK(2);
  }

# #| ( ( |#
# (set-macro-character #\)
#   #'(lambda (stream char)
#       (error "~ of ~: ~ at the beginning of object" 'read stream char)
# )   )
LISPFUNN(rpar_reader,2) # reads )
  {
    var object* stream_ = test_stream_arg(STACK_1);
    pushSTACK(*stream_); # value for Slot STREAM of STREAM-ERROR
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
#                    (error "~: inputstream ~ ends within a String."
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
LISPFUNN(string_reader,2) # reads "
  {
    var object* stream_ = test_stream_arg(STACK_1);
    # stack layout: stream, char.
    if (test_value(S(read_suppress))) { # *READ-SUPPRESS* /= NIL ?
      # yes -> only read ahead of string:
      loop {
        # read next character:
        var object ch;
        var uintWL scode;
        read_char_syntax(ch = ,scode = ,stream_);
        if (scode == syntax_eof) # EOF -> error
          goto fehler_eof;
        if (eq(ch,STACK_0)) # same character as char -> finished
          break;
        if (scode == syntax_single_esc) { # Single-Escape-Character?
          # yes -> read another character:
          read_char_syntax(ch = ,scode = ,stream_);
          if (scode == syntax_eof) # EOF -> error
            goto fehler_eof;
        }
      }
      value1 = NIL; # NIL as value
    } else {
      # no -> really read String
      get_buffers(); # two empty Buffers on the Stack
      # stack layout: stream, char, Buffer, anotherBuffer.
      loop {
        # read next character:
        var object ch;
        var uintWL scode;
        read_char_syntax(ch = ,scode = ,stream_);
        if (scode == syntax_eof) # EOF -> error
          goto fehler_eof;
        if (eq(ch,STACK_2)) # same character as char -> finished
          break;
        if (scode == syntax_single_esc) { # Single-Escape-Character?
          # yes -> read another character:
          read_char_syntax(ch = ,scode = ,stream_);
          if (scode == syntax_eof) # EOF -> error
            goto fehler_eof;
        }
        # push character ch into Buffer:
        ssstring_push_extend(STACK_1,char_code(ch));
      }
      # copy Buffer and convert it into Simple-String:
      {
        var object string;
        #ifndef TYPECODES
        if (TheStream(*stream_)->strmflags & bit(strmflags_immut_bit_B))
          string = coerce_imm_ss(STACK_1);
        else
        #endif
          string = copy_string(STACK_1);
        value1 = string;
      }
      # free Buffer for reuse:
      O(token_buff_2) = popSTACK(); O(token_buff_1) = popSTACK();
    }
    mv_count=1; skipSTACK(2);
    return;
   fehler_eof:
    pushSTACK(*stream_); # value for Slot STREAM of STREAM-ERROR
    pushSTACK(*stream_); # Stream
    pushSTACK(S(read));
    fehler(end_of_file,
           GETTEXT("~: input stream ~ ends within a string")
          );
  }

# reads an Object and creates a list with two elements.
# list2_reader(stream_);
# > stack layout: stream, symbol.
# increases STACK by 2
# modifies STACK, can trigger GC
# can trigger GC
  local Values list2_reader (const object* stream_);
  local Values list2_reader(stream_)
    var const object* stream_;
    {
      var object obj = read_recursive_no_dot(stream_); # read Object
      pushSTACK(obj);
      pushSTACK(allocate_cons()); # second List-cons
      var object new_cons1 = allocate_cons(); # first List-cons
      var object new_cons2 = popSTACK(); # second List-cons
      Car(new_cons2) = popSTACK(); # new_cons2 = (cons obj nil)
      Cdr(new_cons1) = new_cons2; Car(new_cons1) = STACK_0; # new_cons1 = (cons symbol new_cons2)
      value1 = new_cons1; mv_count=1; skipSTACK(2);
    }

# (set-macro-character #\'
#   #'(lambda (stream char)
#       (list 'QUOTE (read stream t nil t))
# )   )
LISPFUNN(quote_reader,2) # reads '
  {
    var object* stream_ = test_stream_arg(STACK_1);
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
LISPFUNN(line_comment_reader,2) # reads ;
  {
    var object* stream_ = test_stream_arg(STACK_1);
    loop {
      var object ch = read_char(stream_); # read character
      if (eq(ch,eof_value) || eq(ch,ascii_char(NL)))
        break;
    }
    value1 = NIL; mv_count=0; skipSTACK(2); # return no values
  }

# ------------------------- READ-Dispatch-Macros ------------------------------

# error-message due to forbidden number at Dispatch-Macros
# fehler_dispatch_zahl(); english: error_dispatch_number();
# > STACK_1: Stream
# > STACK_0: sub-char
  nonreturning_function(local, fehler_dispatch_zahl, (void));
  local void fehler_dispatch_zahl()
    {
      pushSTACK(STACK_1); # Wert for Slot STREAM of STREAM-ERROR
      pushSTACK(STACK_(0+1)); # sub-char
      pushSTACK(STACK_(1+2)); # Stream
      pushSTACK(S(read));
      fehler(stream_error,
             GETTEXT("~ from ~: no number allowed between #"" and $")
            );
    }

# UP: checks the absence of Infix-Argument n
# test_no_infix()
# > stack layout: Stream, sub-char, n.
# > subr_self: Caller (ein SUBR)
# < result: &stream
# increases STACK by 1
# modifies STACK
  local object* test_no_infix (void);
  local object* test_no_infix()
    {
      var object* stream_ = test_stream_arg(STACK_2);
      var object n = popSTACK();
      if ((!nullp(n)) && (!test_value(S(read_suppress))))
        # if n/=NIL and *READ-SUPPRESS*=NIL : report error
        fehler_dispatch_zahl();
      return stream_;
    }

# (set-dispatch-macro-character #\# #\'
#   #'(lambda (stream sub-char n)
#       (when n (error ...))
#       (list 'FUNCTION (read stream t nil t))
# )   )
LISPFUNN(function_reader,3) # reads #'
  {
    var object* stream_ = test_no_infix(); # n must be NIL
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
LISPFUNN(comment_reader,3) # reads #|
  {
    var object* stream_ = test_no_infix(); # n must be NIL
    var uintL depth = 0;
    var object ch;
   loop1:
    ch = read_char(stream_);
   loop2:
    if (eq(ch,eof_value)) # EOF -> Error
      goto fehler_eof;
    elif (eq(ch,STACK_0)) {
      # sub-char has been read
      ch = read_char(stream_); # next character
      if (eq(ch,eof_value)) # EOF -> Error
        goto fehler_eof;
      elif (eq(ch,ascii_char('#'))) {
        # sub-char and '#' has been read -> decrease depth:
        if (depth==0)
          goto fertig;
        depth--;
        goto loop1;
      } else
        goto loop2;
    } elif (eq(ch,ascii_char('#'))) {
      # '#' has been read
      ch = read_char(stream_); # next character
      if (eq(ch,eof_value)) # EOF -> Error
        goto fehler_eof;
      elif (eq(ch,STACK_0)) {
        # '#' and sub-char has been read -> increase depth:
        depth++;
        goto loop1;
      } else
        goto loop2;
    } else
      goto loop1;
   fehler_eof:
    pushSTACK(STACK_1); # value for Slot STREAM of STREAM-ERROR
    pushSTACK(STACK_(0+1)); # sub-char
    pushSTACK(STACK_(0+2)); # sub-char
    pushSTACK(STACK_(1+3)); # Stream
    pushSTACK(S(read));
    fehler(end_of_file,
           GETTEXT("~: input stream ~ ends within a comment #$ ... $#")
          );
   fertig:
    value1 = NIL; mv_count=0; skipSTACK(2); # return no values
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
LISPFUNN(char_reader,3) # reads #\
  {
    # stack layout: Stream, sub-char, n.
    var object* stream_ = test_stream_arg(STACK_2);
    # read Token, with Dummy-Character '\' as start of Token:
    read_token_1(stream_,ascii_char('\\'),syntax_single_esc);
    # finished at once, when *READ-SUPPRESS* /= NIL:
    if (test_value(S(read_suppress))) {
      value1 = NIL; mv_count=1; skipSTACK(3); # NIL as value
      return;
    }
    case_convert_token_1();
    # determine Font:
    if (!nullp(STACK_0)) # n=NIL -> Default-Font 0
      if (!eq(STACK_0,Fixnum_0)) {
        pushSTACK(*stream_); # Wert for Slot STREAM of STREAM-ERROR
        pushSTACK(STACK_(0+1)); # n
        pushSTACK(*stream_); # Stream
        pushSTACK(S(read));
        fehler(stream_error,
               GETTEXT("~ from ~: font number ~ for character is too large, should be = 0")
              );
      }
    # Font ready.
    var object token = O(token_buff_1); # read Token as Semi-Simple-String
    var uintL len = TheIarray(token)->dims[1]; # lengh = Fill-Pointer
    var object hstring = O(displaced_string); # auxiliary string
    TheIarray(hstring)->data = token; # Data-vector := O(token_buff_1)
    token = TheIarray(token)->data; # Normal-Simple-String with Token
    var uintL pos = 0; # current Position in Token
    loop { # search for next Hyphen
      if (len-pos == 1) # Charactername consists of one character?
        break;
      var uintL hyphen = pos; # hyphen := pos
      loop {
        if (hyphen == len) # already at end of Token?
          goto no_more_hyphen;
        if (chareq(TheSstring(token)->data[hyphen],ascii('-'))) # Hyphen found?
          break;
        hyphen++; # no -> continue search
      }
      # Hyphen found at Position hyphen
      var uintL sub_len = hyphen-pos;
      TheIarray(hstring)->dims[0] = pos; # Displaced-Offset := pos
      TheIarray(hstring)->totalsize =
        TheIarray(hstring)->dims[1] = sub_len; # length := hyphen-pos
      # now hstring = (subseq token pos hyphen)
      # Displaced-String hstring is no Bitname -> Error
      pushSTACK(*stream_); # Wert for Slot STREAM of STREAM-ERROR
      pushSTACK(copy_string(hstring)); # copy Displaced-String
      pushSTACK(*stream_); # Stream
      pushSTACK(S(read));
      fehler(stream_error,
             GETTEXT("~ from ~: there is no character bit with name ~")
            );
     bit_ok: # found Bitname, set Bit
      # finished with this Bitname.
      pos = hyphen+1; # go to the next
    }
    # Charactername consists of one letter
    {
      var chart code = TheSstring(token)->data[pos]; # (char token pos)
      value1 = code_char(code); mv_count=1; skipSTACK(3);
      return;
    }
   no_more_hyphen: # no other Hyphen found.
    {
      var uintL sub_len = len-pos; # Length of  Character name
      TheIarray(hstring)->dims[0] = pos; # Displaced-Offset := pos
      /* TheIarray(hstring)->totalsize =          */
      /*   TheIarray(hstring)->dims[1] = sub_len; */ # Länge := len-pos
      # hstring = (subseq token pos hyphen) = restlicher Charactername
      # Test auf Characternamen "CODExxxx" (xxxx Dezimalzahl <256):
      if (sub_len > 4) {
        TheIarray(hstring)->totalsize =
          TheIarray(hstring)->dims[1] = 4;
        # hstring = (subseq token pos (+ pos 4))
        if (!string_equal(hstring,O(charname_prefix))) # = "Code" ?
          goto not_codexxxx; # no -> continue
        # decipher Decimal number:
        var uintL code = 0; # so far read xxxx (<char_code_limit)
        var uintL index = pos+4;
        var const chart* charptr = &TheSstring(token)->data[index];
        loop {
          if (index == len) # reached end of Token?
            break;
          var cint c = as_cint(*charptr++); # next Character
          # is to be digit:
          if (!((c>='0') && (c<='9')))
            goto not_codexxxx;
          code = 10*code + (c-'0'); # add digit
          # code is to be < char_code_limit:
          if (code >= char_code_limit)
            goto not_codexxxx;
          index++;
        }
        # Charactername was of type Typ "Codexxxx" with code = xxxx < char_code_limit
        value1 = code_char(as_chart(code)); mv_count=1; skipSTACK(3);
        return;
      }
     not_codexxxx:
      # Test for Pseudo-Character-Name ^X:
      if ((sub_len == 2) && chareq(TheSstring(token)->data[pos],ascii('^'))) {
        var cint code = as_cint(TheSstring(token)->data[pos+1])-64;
        if (code < 32) {
          value1 = ascii_char(code); mv_count=1; skipSTACK(3);
          return;
        }
      }
      # Test for Charactername like NAME-CHAR:
      TheIarray(hstring)->totalsize =
        TheIarray(hstring)->dims[1] = sub_len; # Length := len-pos
      var object ch = name_char(hstring); # search Character with this Name
      if (nullp(ch)) {
        # not found -> Error
        pushSTACK(*stream_); # value for Slot STREAM of STREAM-ERROR
        pushSTACK(copy_string(hstring)); # copy Charactername
        pushSTACK(*stream_); # Stream
        pushSTACK(S(read));
        fehler(stream_error,
               GETTEXT("~ from ~: there is no character with name ~")
              );
      }
      # found
      value1 = ch; mv_count=1; skipSTACK(3);
      return;
    }
  }

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
  # UP for #B #O #X #R
  # radix_2(base)
  # > base: Basis (>=2, <=36)
  # > stack layout: Stream, sub-char, base.
  # > O(token_buff_1), O(token_buff_2), token_escape_flag: read Token
  # < STACK: cleaned up
  # < mv_space/mv_count: values
  # can trigger GC
  local Values radix_2 (uintWL base);
  local Values radix_2(base)
    var uintWL base;
    {
      # check, if the  Token is a rational number:
      upcase_token(); # convert to upper case
      var object string;
      var zahl_info info;
      switch (test_number_syntax(&base,&string,&info)) {
        case 1: # Integer
          # is last Character a dot?
          if (chareq(TheSstring(string)->data[info.index2-1],ascii('.')))
            # yes -> Decimal-Integer, not in Base base
            goto not_rational;
          # test_number_syntax finished already in step 3,
          # so base is still unchanged.
          skipSTACK(3);
          value1 = read_integer(base,info.sign,string,info.index1,info.index2);
          mv_count=1; return;
        case 2: # Rational
          # test_number_syntax finished already in step 3,
          # so base is still unchanged.
          skipSTACK(3);
          value1 = read_rational(base,info.sign,string,info.index1,info.index3,info.index2);
          mv_count=1; return;
        case 0: # no number
        case 3: # Float
        not_rational: # no rational number
          pushSTACK(STACK_2); # Wert for Slot STREAM of STREAM-ERROR
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
  # UP for #B #O #X
  # radix_1(base)
  # > base: Base (>=2, <=36)
  # > stack layout: Stream, sub-char, n.
  # > subr_self: caller (ein SUBR)
  # < STACK: cleaned
  # < mv_space/mv_count: values
  # can trigger GC
  local Values radix_1 (uintWL base);
  local Values radix_1(base)
    var uintWL base;
    {
      var object* stream_ = test_stream_arg(STACK_2);
      read_token(stream_); # read Token
      # finished at once when *READ-SUPPRESS* /= NIL:
      if (test_value(S(read_suppress))) {
        value1 = NIL; mv_count=1; skipSTACK(3); # NIL as value
        return;
      }
      if (!nullp(popSTACK())) # n/=NIL -> Error
        fehler_dispatch_zahl();
      pushSTACK(fixnum(base)); # base as Fixnum
      return_Values radix_2(base);
    }

# (set-dispatch-macro-character #\# #\B
#   #'(lambda (stream sub-char n) (radix-1 stream sub-char n 2))
# )
LISPFUNN(binary_reader,3) # reads #B
  {
    return_Values radix_1(2);
  }

# (set-dispatch-macro-character #\# #\O
#   #'(lambda (stream sub-char n) (radix-1 stream sub-char n 8))
# )
LISPFUNN(octal_reader,3) # reads #O
  {
    return_Values radix_1(8);
  }

# (set-dispatch-macro-character #\# #\X
#   #'(lambda (stream sub-char n) (radix-1 stream sub-char n 16))
# )
LISPFUNN(hexadecimal_reader,3) # reads #X
  {
    return_Values radix_1(16);
  }

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
LISPFUNN(radix_reader,3) # reads #R
  {
    var object* stream_ = test_stream_arg(STACK_2);
    read_token(stream_); # read Token
    # finished at once when *READ-SUPPRESS* /= NIL:
    if (test_value(S(read_suppress))) {
      value1 = NIL; mv_count=1; skipSTACK(3); # NIL as value
      return;
    }
    # check n:
    if (nullp(STACK_0)) {
      pushSTACK(*stream_); # value for Slot STREAM of STREAM-ERROR
      pushSTACK(*stream_); # Stream
      pushSTACK(S(read));
      fehler(stream_error,
             GETTEXT("~ from ~: the number base must be given between #"" and R")
            );
    }
    var uintL base;
    # n must be a Fixnum between 2 and 36 (inclusive):
    if (posfixnump(STACK_0) &&
        (base = posfixnum_to_L(STACK_0), (base >= 2) && (base <= 36))
       ) {
      return_Values radix_2(base); # interprete Token as rational number
    } else {
      pushSTACK(*stream_); # value for Slot STREAM of STREAM-ERROR
      pushSTACK(STACK_(0+1)); # n
      pushSTACK(*stream_); # Stream
      pushSTACK(S(read));
      fehler(stream_error,
             GETTEXT("~ from ~: The base ~ given between #"" and R should lie between 2 and 36")
            );
    }
  }

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
LISPFUNN(complex_reader,3) # reads #C
  {
    var object* stream_ = test_no_infix(); # n must be NIL
    var object obj = read_recursive_no_dot(stream_); # read next Object
    # finished at once when *READ-SUPPRESS* /= NIL:
    if (test_value(S(read_suppress))) {
      value1 = NIL; mv_count=1; skipSTACK(2); # NIL as value
      return;
    }
    obj = make_references(obj); # unentangle references untimely
    # check, if this is a 2-elemnt List of real numbers:
    if (!consp(obj)) goto bad; # obj must be a Cons !
    {
      var object obj2 = Cdr(obj);
      if (!consp(obj2)) goto bad; # obj2 must be a Cons!
      if (!nullp(Cdr(obj2))) goto bad; # with (cdr obj2) = nil !
      if_realp(Car(obj), ; , goto bad; ); # and (car obj) being a real number!
      if_realp(Car(obj2), ; , goto bad; ); # and (car obj2) being a real number!
      # execute (apply #'COMPLEX obj):
      apply(L(complex),0,obj);
      mv_count=1; skipSTACK(2); return; # value1 as value
    }
   bad:
    pushSTACK(*stream_); # value for Slot STREAM of STREAM-ERROR
    pushSTACK(obj); # Object
    pushSTACK(*stream_); # Stream
    pushSTACK(S(read));
    fehler(stream_error,
           GETTEXT("~ from ~: bad syntax for complex number: #C~")
          );
  }

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
LISPFUNN(uninterned_reader,3) # reads #:
  {
    var object* stream_ = test_stream_arg(STACK_2);
    # when *READ-SUPPRESS* /= NIL, read form and return NIL:
    if (test_value(S(read_suppress))) {
      read_recursive(stream_);
      value1 = NIL; mv_count=1; skipSTACK(3); return;
    }
    # read next character:
    {
      var object ch;
      var uintWL scode;
      read_char_syntax(ch = ,scode = ,stream_);
      if (scode == syntax_eof) # EOF -> Error
        fehler_eof_innen(stream_);
      if (scode > syntax_constituent) {
        # no character, that is allowed at beginning of Token -> Error
        pushSTACK(*stream_); # value for Slot STREAM of STREAM-ERROR
        pushSTACK(*stream_); # Stream
        pushSTACK(S(read));
        fehler(stream_error,
               GETTEXT("~ from ~: token expected after #:")
              );
      }
      # read Token until the end:
      read_token_1(stream_,ch,scode);
      case_convert_token_1();
    }
    if (!nullp(popSTACK())) # n/=NIL -> Error
      fehler_dispatch_zahl();
    # copy Token and convert into Simple-String:
    var object string = coerce_imm_ss(O(token_buff_1));
    # test for Package-Marker:
    {
      var object buff_2 = O(token_buff_2); # Attribut-Code-Buffer
      var uintL len = TheIarray(buff_2)->dims[1]; # length = Fill-Pointer
      if (len > 0) {
        var uintB* attrptr = &TheSbvector(TheIarray(buff_2)->data)->data[0];
        # Test, if one of the len Attribut-Codes starting at attrptr and afterwards is an a_pack_m:
        dotimespL(len,len, { if (*attrptr++ == a_pack_m) goto fehler_dopp; } );
      }
    }
    # build uninterned Symbol with this Name:
    value1 = make_symbol(string); mv_count=1; skipSTACK(2); return;
   fehler_dopp:
    pushSTACK(*stream_); # value for Slot STREAM of STREAM-ERROR
    pushSTACK(string); # Token
    pushSTACK(*stream_); # Stream
    pushSTACK(S(read));
    fehler(stream_error,
           GETTEXT("~ from ~: token ~ after #: should contain no colon")
          );
  }

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
  {
    var object* stream_ = test_stream_arg(STACK_2);
    read_token(stream_); # read Token
    # finished at once, if *READ-SUPPRESS* /= NIL:
    if (test_value(S(read_suppress))) {
      value1 = NIL; mv_count=1; skipSTACK(3); # NIL as value
      return;
    }
    # Test, if no Escape-character and only 0s and 1s are used:
    if (token_escape_flag) {
     fehler_nur01:
      pushSTACK(*stream_); # value for Slot STREAM of STREAM-ERROR
      pushSTACK(*stream_); # Stream
      pushSTACK(S(read));
      fehler(stream_error,
             GETTEXT("~ from ~: only zeroes and ones are allowed after #*")
            );
    }
    var object buff_1 = O(token_buff_1); # Character-Buffer
    var uintL len = TheIarray(buff_1)->dims[1]; # length = Fill-Pointer
    if (len > 0) {
      var const chart* charptr = &TheSstring(TheIarray(buff_1)->data)->data[0];
      var uintL count;
      dotimespL(count,len, {
        var chart c = *charptr++; # next Character
        if (!(chareq(c,ascii('0')) || chareq(c,ascii('1')))) # only '0' und '1' are OK
          goto fehler_nur01;
      });
    }
    # check n:
    var uintL n; # Length of Bitvektors
    if (nullp(STACK_0)) {
      n = len; # Defaultvalue is the Tokenlength
    } else {
      # n specified, an Integer >=0.
      n = (posfixnump(STACK_0) ? posfixnum_to_L(STACK_0) # Fixnum -> value
                               : bitm(oint_data_len)-1 # Bignum -> big value
          );
      if (n<len) {
        pushSTACK(*stream_); # value for Slot STREAM of STREAM-ERROR
        pushSTACK(STACK_(0+1)); # n
        pushSTACK(*stream_); # Stream
        pushSTACK(S(read));
        fehler(stream_error,
               GETTEXT("~ from ~: bit vector is longer than the explicitly given length ~")
              );
      }
      if ((n>0) && (len==0)) {
        pushSTACK(*stream_); # value for Slot STREAM of STREAM-ERROR
        pushSTACK(STACK_(0+1)); # n
        pushSTACK(*stream_); # Stream
        pushSTACK(S(read));
        fehler(stream_error,
               GETTEXT("~ from ~: must specify element of bit vector of length ~")
              );
      }
    }
    # create new Bit-Vector with length n:
    var object bv = allocate_bit_vector(Atype_Bit,n);
    # and fill the Bits into it:
    buff_1 = O(token_buff_1);
    {
      var const chart* charptr = &TheSstring(TheIarray(buff_1)->data)->data[0];
      var chart ch; # last character ('0' or '1')
      var uintL index = 0;
      while (index < n) {
        if (index < len)
          ch = *charptr++; # possibly, fetch next Character
        if (chareq(ch,ascii('0'))) {
          sbvector_bclr(bv,index); # Null -> delete Bit
        } else {
          sbvector_bset(bv,index); # Eins -> set Bit
        }
        index++;
      }
    }
    value1 = bv; mv_count=1; skipSTACK(3); # bv as value
  }

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
LISPFUNN(vector_reader,3) # reads #(
  {
    var object* stream_ = test_stream_arg(STACK_2);
    # read List until parenthese, Dot is not allowed:
    var object elements = read_delimited_list(stream_,ascii_char(')'),eof_value);
    # already finished when *READ-SUPPRESS* /= NIL:
    if (test_value(S(read_suppress))) {
      value1 = NIL; mv_count=1; skipSTACK(3); # NIL as value
      return;
    }
    var uintL len = llength(elements); # Listlength
    # check n:
    var uintL n; # Length of Vector
    if (nullp(STACK_0)) {
      n = len; # Defaultvalue is the length of the Token
    } else {
      # specify n, an Integer >=0.
      n = (posfixnump(STACK_0) ? posfixnum_to_L(STACK_0) # Fixnum -> value
                               : bitm(oint_data_len)-1 # Bignum -> big value
          );
      if (n<len) {
        pushSTACK(*stream_); # value for Slot STREAM of STREAM-ERROR
        pushSTACK(STACK_(0+1)); # n
        pushSTACK(*stream_); # Stream
        pushSTACK(S(read));
        fehler(stream_error,
               GETTEXT("~ from ~: vector is longer than the explicitly given length ~")
              );
      }
      if ((n>0) && (len==0)) {
        pushSTACK(*stream_); # value for Slot STREAM of STREAM-ERROR
        pushSTACK(STACK_(0+1)); # n
        pushSTACK(*stream_); # Stream
        pushSTACK(S(read));
        fehler(stream_error,
               GETTEXT("~ from ~: must specify element of vector of length ~")
              );
      }
    }
    # create new Vector with Length n:
    pushSTACK(elements); # save List
    var object v = allocate_vector(n);
    elements = popSTACK(); # retrieve List
    # und fill it with the Elements:
    {
      var object* vptr = &TheSvector(v)->data[0];
      var object el; # last Element
      var uintL index = 0;
      while (index < n) {
        if (index < len) {
          el = Car(elements); elements = Cdr(elements); # possibly fetch next Element
        }
        *vptr++ = el;
        index++;
      }
    }
    value1 = v; mv_count=1; skipSTACK(3); # v as value
  }

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
LISPFUNN(array_reader,3) # reads #A
  {
    var object* stream_ = test_stream_arg(STACK_2);
    # stack layout: stream, sub-char, n.
    if (test_value(S(read_suppress))) { # *READ-SUPPRESS* /= NIL ?
      # yes -> skip next Object:
      read_recursive_no_dot(stream_);
      value1 = NIL; mv_count=1; skipSTACK(3); return;
    }
    if (nullp(STACK_0)) { # n not specified?
      # yes -> read List (eltype dims contents):
      var object obj = read_recursive_no_dot(stream_); # read List
      obj = make_references(obj); # unentangle references
      # (this is harmless, since we don't use this #A-Syntax
      # for Arrays with Elementtyp T, and Byte-Arrays contain no references.)
      if (!consp(obj)) goto bad;
      {
        var object obj2 = Cdr(obj);
        if (!consp(obj2)) goto bad;
        var object obj3 = Cdr(obj2);
        if (!consp(obj3)) goto bad;
        if (!nullp(Cdr(obj3))) goto bad;
        # call (MAKE-ARRAY dims :element-type eltype :initial-contents contents):
        STACK_2 = Car(obj2); STACK_1 = S(Kelement_type); STACK_0 = Car(obj);
        pushSTACK(S(Kinitial_contents)); pushSTACK(Car(obj3));
        goto call_make_array;
      }
     bad:
      pushSTACK(*stream_); # value for Slot STREAM of STREAM-ERROR
      pushSTACK(obj); # Object
      pushSTACK(*stream_); # Stream
      pushSTACK(S(read));
      fehler(stream_error,
             GETTEXT("~ from ~: bad syntax for array: #A~")
            );
    }
    # n specifies the Rank of the Arrays.
    # read content:
    {
      dynamic_bind(S(backquote_level),NIL); # bind SYS::*BACKQUOTE-LEVEL* to NIL
      var object contents = read_recursive_no_dot(stream_);
      dynamic_unbind();
      pushSTACK(contents); pushSTACK(contents);
    }
    STACK_4 = NIL; # dims := '()
    # stack layout: dims, -, rank, subcontents, contents.
    # determine Dimensions and Element-type:
    if (eq(STACK_2,Fixnum_0)) { # rank=0 ?
      STACK_2 = S(t); # yes -> eltype := 'T
    } else {
      var object i = Fixnum_0; # former nesting depth
      loop {
        pushSTACK(STACK_1); funcall(L(length),1); # (LENGTH subcontents)
        # push on dims:
        STACK_3 = value1;
        {
          var object new_cons = allocate_cons();
          Car(new_cons) = STACK_3; Cdr(new_cons) = STACK_4;
          STACK_4 = new_cons;
        }
        # increase depth:
        i = fixnum_inc(i,1); if (eql(i,STACK_2)) break;
        # first Element of subcontents for the following Dimensionen:
        if (!eq(STACK_3,Fixnum_0)) { # (only if (length subcontents) >0)
          pushSTACK(STACK_1); pushSTACK(Fixnum_0); funcall(L(elt),2);
          STACK_1 = value1; # subcontents := (ELT subcontents 0)
        }
      }
      nreverse(STACK_4); # reverse List dims
      # determine eltype according to innermost subcontents:
      STACK_2 = (stringp(STACK_1) ? S(character) :          # String: CHARACTER
                 bit_vector_p(Atype_Bit,STACK_1) ? S(bit) : # Bitvector: BIT
                 S(t)                                       # else (Liste): T
                );
    }
    # stack layout: dims, -, eltype, -, contents.
    # call MAKE-ARRAY:
    STACK_3 = S(Kelement_type); STACK_1 = S(Kinitial_contents);
    call_make_array:
    funcall(L(make_array),5);
    mv_count=1; return;
  }

# Errormessage for #. and #, because of *READ-EVAL*.
# fehler_read_eval_forbidden(&stream,obj); english: erro_read_eval_forbidden(&stream,obj);
# > stream: Stream
# > obj: Object, whose Evaluation was examined
  nonreturning_function(local, fehler_read_eval_forbidden, (object* stream_, object obj));
  local void fehler_read_eval_forbidden(stream_,obj)
    var object* stream_;
    var object obj;
    {
      pushSTACK(*stream_); # value for Slot STREAM of STREAM-ERROR
      pushSTACK(obj); # Object
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
LISPFUNN(read_eval_reader,3) # reads #.
  {
    var object* stream_ = test_stream_arg(STACK_2);
    var object obj = read_recursive_no_dot(stream_); # read Form
    # if *READ-SUPPRESS* /= NIL ==> finished immediately:
    if (test_value(S(read_suppress))) {
      value1 = NIL; mv_count=1; skipSTACK(3);
      return;
    }
    if (!nullp(popSTACK())) # n/=NIL -> Error
      fehler_dispatch_zahl();
    obj = make_references(obj); # unentangle references
    # either *READ-EVAL* or the Stream must allow the Evaluation.
    if (!(test_value(S(read_eval)) || stream_get_read_eval(*stream_)))
      fehler_read_eval_forbidden(stream_,obj);
    eval_noenv(obj); # evaluate Form
    mv_count=1; skipSTACK(2); # only 1 value back
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
LISPFUNN(load_eval_reader,3) # reads #,
  {
    var object* stream_ = test_stream_arg(STACK_2);
    var object obj = read_recursive_no_dot(stream_); # read Form
    # finished immediately, when *READ-SUPPRESS* /= NIL:
    if (test_value(S(read_suppress))) {
      value1 = NIL; mv_count=1; skipSTACK(3);
      return;
    }
    if (!nullp(popSTACK())) # n/=NIL -> Error
      fehler_dispatch_zahl();
    obj = make_references(obj); # unentangle references
    if (test_value(S(compiling))) {
      # In Compiler:
      pushSTACK(obj);
      var object newobj = allocate_loadtimeeval(); # Load-time-Eval-Object
      TheLoadtimeeval(newobj)->loadtimeeval_form = popSTACK(); # with obj as Form
      value1 = newobj;
    } else {
      # In Interpreter:
      # either *READ-EVAL* or the Stream must allow the Evaluation.
      if (!(test_value(S(read_eval)) || stream_get_read_eval(*stream_)))
        fehler_read_eval_forbidden(stream_,obj);
      eval_noenv(obj); # evaluate Form
    }
    mv_count=1; skipSTACK(2); # only 1 value back
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

# UP: creates an internal Label and looks it up in *READ-REFERENCE-TABLE*.
# lookup_label()
# > stack layout: Stream, sub-char, n.
# < result: (or (assoc label sys::*read-reference-table* :test #'eq) label)
  local object lookup_label (void);
  local object lookup_label()
    {
      var object n = STACK_0;
      if (nullp(n)) { # not specified?
        pushSTACK(STACK_2); # Wert for Slot STREAM of STREAM-ERROR
        pushSTACK(STACK_(1+1)); # sub-char
        pushSTACK(STACK_(2+2)); # Stream
        pushSTACK(S(read));
        fehler(stream_error,
               GETTEXT("~ from ~: a number must be given between #"" and $")
              );
      }
      # n is an Integer >=0
      if (!read_label_integer_p(n)) {
        # n is too big
        pushSTACK(STACK_2); # Value for Slot STREAM of STREAM-ERROR
        pushSTACK(STACK_(1+1)); # sub-char
        pushSTACK(STACK_(0+2)); # n
        pushSTACK(STACK_(2+3)); # Stream
        pushSTACK(S(read));
        fehler(stream_error,
               GETTEXT("~ from ~: label #~? too large")
              );
      }
      var object label = make_read_label(posfixnum_to_L(n)); # Internal-Label with Nummer n
      var object alist = # value of SYS::*READ-REFERENCE-TABLE*
        Symbol_value(S(read_reference_table));
      # execute (assoc label alist :test #'eq):
      while (consp(alist)) {
        var object acons = Car(alist); # List-element
        if (!consp(acons)) goto bad_reftab; # must be a Cons !
        if (eq(Car(acons),label)) # its CAR = label ?
          return acons; # yes -> fertig
        alist = Cdr(alist);
      }
      if (nullp(alist)) # List-end with NIL ?
        return label; # yes -> (assoc ...) = NIL -> finished with label
     bad_reftab: # value of SYS::*READ-REFERENCE-TABLE* is no Alist
      pushSTACK(Symbol_value(S(read_reference_table))); # value of SYS::*READ-REFERENCE-TABLE*
      pushSTACK(S(read_reference_table)); # SYS::*READ-REFERENCE-TABLE*
      pushSTACK(STACK_(2+2)); # Stream
      pushSTACK(S(read));
      fehler(error,
             GETTEXT("~ from ~: the value of ~ has been altered arbitrarily, it is not an alist: ~")
            );
    }

LISPFUNN(label_definition_reader,3) # reads #=
  {
    # when *READ-SUPPRESS* /= NIL, #n= is treated as comment:
    if (test_value(S(read_suppress))) {
      value1 = NIL; mv_count=0; skipSTACK(3); # no values
      return;
    }
    # create Label and lookup in Table:
    var object lookup = lookup_label();
    if (consp(lookup)) {
      # found -> has already been there -> error:
      pushSTACK(STACK_2); # value for Slot STREAM of STREAM-ERROR
      pushSTACK(STACK_(0+1)); # n
      pushSTACK(STACK_(2+2)); # Stream
      pushSTACK(S(read));
      fehler(stream_error,
             GETTEXT("~ from ~: label #~= may not be defined twice")
            );
    } else {
      # lookup = label, not jeopardized by GC.
      # (push (setq h (cons label label)) sys::*read-reference-table*) :
      var object* stream_ = test_stream_arg(STACK_2);
      {
        var object new_cons = allocate_cons();
        Car(new_cons) = Cdr(new_cons) = lookup; # h = (cons label label)
        pushSTACK(new_cons); # save h
      }
      {
        var object new_cons = allocate_cons(); # new List-Cons
        Car(new_cons) = STACK_0;
        Cdr(new_cons) = Symbol_value(S(read_reference_table));
        Symbol_value(S(read_reference_table)) = new_cons;
      }
      var object obj = read_recursive_no_dot(stream_); # read Objekt
      var object h = popSTACK();
      if (eq(obj,Car(h))) { # read Objekt = (car h) = label ?
        # yes -> cyclic Definition -> Error
        pushSTACK(*stream_); # Wert for Slot STREAM of STREAM-ERROR
        pushSTACK(STACK_(0+1)); # n
        pushSTACK(STACK_(0+2)); # n
        pushSTACK(*stream_); # Stream
        pushSTACK(S(read));
        fehler(stream_error,
               GETTEXT("~ from ~: #~= #~#"" is illegal")
              );
      }
      # insert read Objekt as (cdr h):
      Cdr(h) = obj;
      value1 = obj; mv_count=1; skipSTACK(3); # obj as value
    }
  }

LISPFUNN(label_reference_reader,3) # reads ##
  {
    # when *READ-SUPPRESS* /= NIL, finished immediately:
    if (test_value(S(read_suppress))) {
      value1 = NIL; mv_count=1; skipSTACK(3);
      return;
    }
    # construct Label and lookup in Table:
    var object lookup = lookup_label();
    if (consp(lookup)) {
      # found -> return Label as read object:
      value1 = Car(lookup); mv_count=1; skipSTACK(3);
    } else {
      # not found
      pushSTACK(STACK_2); # value for Slot STREAM of STREAM-ERROR
      pushSTACK(STACK_(0+1)); # n
      pushSTACK(STACK_(2+2)); # Stream
      pushSTACK(S(read));
      fehler(stream_error,
             GETTEXT("~ from ~: undefined label #~#")
            );
    }
  }

# (set-dispatch-macro-character #\# #\<
#   #'(lambda (stream sub-char n)
#       (error "~ von ~: Als #<...> ausgegebene Objekte sind nicht mehr einlesbar."
#               'read stream
# )   ) )
LISPFUNN(not_readable_reader,3) # reads #<
  {
    var object* stream_ = test_stream_arg(STACK_2);
    pushSTACK(*stream_); # value for Slot STREAM of STREAM-ERROR
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
LISPFUNN(syntax_error_reader,3) # reads #) and #whitespace
  {
    var object* stream_ = test_stream_arg(STACK_2);
    pushSTACK(*stream_); # value for Slot STREAM of STREAM-ERROR
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

# UP: checks, if Feature-Expression is fulfilled.
# interpret_feature(expr)
# > expr: a Feature-Expresion
# > STACK_1: Stream
# < result: truth value: 0 if fulfilled, ~0 if not fulfilled.
  local uintWL interpret_feature (object expr);
  local uintWL interpret_feature(expr)
    var object expr;
    {
      check_SP();
      if (symbolp(expr)) {
        # expr Symbol, search in *FEATURES*:
        var object list = Symbol_value(S(features)); # value of *FEATURES*
        while (consp(list)) {
          if (eq(Car(list),expr))
            goto ja;
          list = Cdr(list);
        }
        goto nein;
      } elif (consp(expr) && symbolp(Car(expr))) {
        var object opname = Symbol_name(Car(expr));
        var uintWL and_or_flag;
        if (string_gleich(opname,Symbol_name(S(and)))) {
          # expr = (AND ...)
          and_or_flag = 0; goto and_or;
        } elif (string_gleich(opname,Symbol_name(S(or)))) {
          # expr = (OR ...)
          and_or_flag = ~0;
         and_or:
          # interprete on List-elements of expr, until there is a
          # result /=and_or_flag. Default is and_or_flag.
          var object list = Cdr(expr);
          while (consp(list)) {
            # interprete on List-element:
            var uintWL sub_erg = interpret_feature(Car(list));
            if (!(sub_erg == and_or_flag))
              return sub_erg;
            list = Cdr(list);
          }
          if (nullp(list))
            return and_or_flag;
          # expr was a  Dotted List -> error
        } elif (string_gleich(opname,Symbol_name(S(not)))) {
          # expr = (NOT ...) is to be of the shape (NOT obj):
          var object opargs = Cdr(expr);
          if (consp(opargs) && nullp(Cdr(opargs)))
            return ~interpret_feature(Car(opargs));
          # expr has no correct shape -> error
        }
        # wrong (car expr) -> error
      }
     bad: # wrong structure of Feature-Expression
      pushSTACK(STACK_1); # value for Slot STREAM of STREAM-ERROR
      pushSTACK(expr); # Feature-Expression
      pushSTACK(STACK_(1+2)); # Stream
      pushSTACK(S(read));
      fehler(stream_error,
             GETTEXT("~ from ~: illegal feature ~")
            );
     ja: return 0; # expr is fulfilled
     nein: return ~0; # expr is not fulfilled
    }

# UP for #+ und #-
# feature(sollwert)
# > expected value: exprected truth value of Feature-Expression
# > Stack Structure: Stream, sub-char, n.
# > subr_self: caller (a SUBR)
# < STACK: increased by 3
# < mv_space/mv_count: values
# can trigger GC
  local Values feature (uintWL sollwert);
  local Values feature(sollwert)
    var uintWL sollwert;
    {
      var object* stream_ = test_no_infix(); # n must be NIL
      dynamic_bind(S(read_suppress),NIL); # bind *READ-SUPPRESS* to NIL
      dynamic_bind(S(packagestern),O(keyword_package)); # bind *PACKAGE* to #<PACKAGE KEYWORD>
      var object expr = read_recursive_no_dot(stream_); # read Feature-Expression
      dynamic_unbind();
      dynamic_unbind();
      # interprete Feature-Expression:
      expr = make_references(expr); # first unentangle references
      if (interpret_feature(expr) == sollwert) {
        # truth value "true"
        # read next Objekt and set for value:
        value1 = read_recursive_no_dot(stream_); mv_count=1;
      } else {
        # truth value "false"
        # bind *READ-SUPPRESS* to T, read Objekt, comment
        dynamic_bind(S(read_suppress),T);
        read_recursive_no_dot(stream_);
        dynamic_unbind();
        value1 = NIL; mv_count=0; # no values
      }
      skipSTACK(2);
    }

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
LISPFUNN(feature_reader,3) # reads #+
  {
    return_Values feature(0);
  }

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
LISPFUNN(not_feature_reader,3) # reads #-
  {
    return_Values feature(~0);
  }

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
LISPFUNN(structure_reader,3) # reads #S
  {
    var object* stream_ = test_no_infix(); # n must be NIL
    # when *READ-SUPPRESS* /= NIL, only read one object:
    if (test_value(S(read_suppress))) {
      read_recursive_no_dot(stream_); # read Objekt and throw away,
      value1 = NIL; mv_count=1; skipSTACK(2); return; # NIL as value
    }
    # bind SYS::*BACKQUOTE-LEVEL* to NIL and read object:
    dynamic_bind(S(backquote_level),NIL);
    var object args = read_recursive_no_dot(stream_);
    dynamic_unbind();
    # check read List:
    if (atomp(args)) {
      pushSTACK(*stream_); # value for Slot STREAM of STREAM-ERROR
      pushSTACK(args); # Arguments
      pushSTACK(*stream_); # Stream
      pushSTACK(S(read));
      fehler(stream_error,
             GETTEXT("~ from ~: #S must be followed by the type and the contents of the structure, not ~")
            );
    }
    {
      var object name = Car(args); # Type of Structure
      STACK_0 = args = Cdr(args); # save Restlist
      # Stack Structure: Stream, remaining Args.
      if (!symbolp(name)) { # Type must be a Symbol !
        pushSTACK(*stream_); # value for Slot STREAM of STREAM-ERROR
        pushSTACK(name);
        pushSTACK(*stream_); # Stream
        pushSTACK(S(read));
        fehler(stream_error,
               GETTEXT("~ from ~: the type of a structure should be a symbol, not ~")
              );
      }
      pushSTACK(name);
      # Stack Structure: Stream, remaining Args, name.
      if (eq(name,S(hash_table))) { # Symbol HASH-TABLE ?
        # yes -> treat specially, no Structure:
        # Hash-Tabelle
        # Remaining Argumentlist must be a Cons:
        if (!consp(args)) {
          pushSTACK(*stream_); # value for Slot STREAM of STREAM-ERROR
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
        funcall(L(make_hash_table),4); # build Hash-Table
        mv_count=1; # value1 as value
        skipSTACK(3); return;
      }
      if (eq(name,S(random_state))) { # Symbol RANDOM-STATE ?
        # yes -> treat specially, no Structure:
        # Random-State
        # Remaining Argumentlist must be a Cons with NIL as CDR and
        # a Simple-Bit-Vector of length 64 as CAR:
        if (!(consp(args)
              && nullp(Cdr(args))
              && simple_bit_vector_p(Atype_Bit,Car(args))
              && (Sbvector_length(Car(args)) == 64)
           ) ) {
          pushSTACK(*stream_); # value for Slot STREAM of STREAM-ERROR
          pushSTACK(name);
          pushSTACK(*stream_); # Stream
          pushSTACK(S(read));
          fehler(stream_error,
                 GETTEXT("~ from ~: bad ~")
                );
        }
        STACK_0 = Car(args); # save Simple-Bit-Vector
        var object ergebnis = allocate_random_state(); # new Random-State
        The_Random_state(ergebnis)->random_state_seed = popSTACK(); # fill
        value1 = ergebnis; mv_count=1; skipSTACK(2); return;
      }
      if (eq(name,S(pathname))) { # Symbol PATHNAME ?
        # yes -> treat specially, no Structure:
        STACK_1 = make_references(args); pushSTACK(L(make_pathname));
      }
      #ifdef LOGICAL_PATHNAMES
      elif (eq(name,S(logical_pathname))) { # Symbol LOGICAL-PATHNAME ?
        # yes -> treat specially, no Structure:
        STACK_1 = make_references(args); pushSTACK(L(make_logical_pathname));
      }
      #endif
      elif (eq(name,S(byte))) { # Symbol BYTE ?
        # yes -> treat specially, no Structure:
        pushSTACK(S(make_byte));
      }
      else {
        # execute (GET name 'SYS::DEFSTRUCT-DESCRIPTION):
        var object description = get(name,S(defstruct_description));
        if (eq(description,unbound)) { # nothing found?
          # Structure of this Type undefined
          pushSTACK(*stream_); # value for Slot STREAM of STREAM-ERROR
          pushSTACK(name);
          pushSTACK(*stream_); # Stream
          pushSTACK(S(read));
          fehler(stream_error,
                 GETTEXT("~ from ~: no structure of type ~ has been defined")
                );
        }
        # description must be a Simple-Vector of length >=4:
        if (!(simple_vector_p(description) && (Svector_length(description) >= 4))) {
          pushSTACK(*stream_); # value for Slot STREAM of STREAM-ERROR
          pushSTACK(name);
          pushSTACK(S(defstruct_description));
          pushSTACK(*stream_); # Stream
          pushSTACK(S(read));
          fehler(stream_error,
                 GETTEXT("~ from ~: bad ~ for ~")
                );
        }
        # fetch constructor-function:
        var object constructor = # (svref description 2)
          TheSvector(description)->data[2];
        if (nullp(constructor)) {
          pushSTACK(*stream_); # value for Slot STREAM of STREAM-ERROR
          pushSTACK(name);
          pushSTACK(*stream_); # Stream
          pushSTACK(S(read));
          fehler(stream_error,
                 GETTEXT("~ from ~: structures of type ~ cannot be read in, missing constructor function")
                );
        }
    # call constructor-function with adapted Argumentlist:
        pushSTACK(constructor);
      }
    }
    # stack layout: Stream, remaining Args, name, constructor.
    var uintC argcount = 0; # number of arguments for constructor
    loop { # process remaining Argumentlist,
           # push Arguments for constructor on STACK:
      check_STACK();
      args = *(stream_ STACKop -1); # remaining Args
      if (nullp(args)) # no more -> Arguments in STACK are ready
        break;
      if (atomp(args)) {
       dotted:
        pushSTACK(*stream_); # value for Slot STREAM of STREAM-ERROR
        pushSTACK(*(stream_ STACKop -2)); # name
        pushSTACK(*stream_); # Stream
        pushSTACK(S(read));
        fehler(stream_error,
               GETTEXT("~ from ~: a structure ~ may not contain a component \".\"")
              );
      }
      {
        var object slot = Car(args);
        if (!symbolp(slot)) {
          pushSTACK(*stream_); # value for Slot STREAM of STREAM-ERROR
          pushSTACK(*(stream_ STACKop -2)); # name
          pushSTACK(slot);
          pushSTACK(*stream_); # Stream
          pushSTACK(S(read));
          fehler(stream_error,
                 GETTEXT("~ from ~: ~ is not a symbol, not a slot name of structure ~")
                );
        }
        if (nullp(Cdr(args))) {
          pushSTACK(*stream_); # value for Slot STREAM of STREAM-ERROR
          pushSTACK(*(stream_ STACKop -2)); # name
          pushSTACK(slot);
          pushSTACK(*stream_); # Stream
          pushSTACK(S(read));
          fehler(stream_error,
                 GETTEXT("~ from ~: missing value of slot ~ in structure ~")
                );
        }
        if (matomp(Cdr(args)))
          goto dotted;
        {
          var object kw = intern_keyword(Symbol_name(slot)); # Slotname as Keyword
          pushSTACK(kw); # Keyword into STACK
        }
      }
      args = *(stream_ STACKop -1); # again the same remaining Args
      args = Cdr(args);
      pushSTACK(Car(args)); # Slot-value into STACK
      *(stream_ STACKop -1) = Cdr(args); # shorten Arglist
      argcount += 2; # and count
      if (argcount == 0) {
        # Argument-Counter has become too big
        pushSTACK(*stream_); # value for Slot STREAM of STREAM-ERROR
        pushSTACK(*(stream_ STACKop -2)); # name
        pushSTACK(*stream_); # Stream
        pushSTACK(S(read));
        fehler(stream_error,
               GETTEXT("~ from ~: too many slots for structure ~")
              );
      }
    }
    funcall(*(stream_ STACKop -3),argcount); # call constructor
    mv_count=1; skipSTACK(4); return; # value1 as value
  }

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

  # error-message because of wrong Syntax of a Code-Vector
  # fehler_closure_badchar(); english: error_closure_badchar();
  # > stack layout: stream, sub-char, arg.
    nonreturning_function(local, fehler_closure_badchar, (void));
    local void fehler_closure_badchar()
      {
        pushSTACK(STACK_2); # value for Slot STREAM of STREAM-ERROR
        pushSTACK(STACK_(0+1)); # n
        pushSTACK(STACK_(2+2)); # Stream
        pushSTACK(S(read));
        fehler(stream_error,
               GETTEXT("~ from ~: illegal syntax of closure code vector after #~Y")
              );
      }

  # UP: checks, if Character ch with Syntaxcode scode is a
  # Hexadecimal-Digit, and delivers its value.
  # hexziffer(ch,scode) english: hexdigit(ch,scode)
  # > ch, scode: Character (or eof_value) and its Syntaxcode
  # > stack layout: stream, sub-char, arg.
  # < Result: value (>=0, <16) of Hexdigit
    local uintB hexziffer (object ch, uintWL scode);
    local uintB hexziffer(ch,scode)
      var object ch;
      var uintWL scode;
      {
        if (scode == syntax_eof)
          fehler_eof_innen(&STACK_2);
        # ch is a Character
        var cint c = as_cint(char_code(ch));
        if (c<'0') goto badchar; if (c<='9') { return (c-'0'); } # '0'..'9'
        if (c<'A') goto badchar; if (c<='F') { return (c-'A'+10); } # 'A'..'F'
        if (c<'a') goto badchar; if (c<='f') { return (c-'a'+10); } # 'a'..'f'
       badchar: fehler_closure_badchar();
      }

LISPFUNN(closure_reader,3) # liest #Y
  {
    var object* stream_ = test_stream_arg(STACK_2);
    # when n=0 read an Encoding:
    if (eq(STACK_0,Fixnum_0)) {
      dynamic_bind(S(read_suppress),NIL); # bind *READ-SUPPRESS* to NIL
      dynamic_bind(S(packagestern),O(charset_package)); # bind *PACKAGE* to #<PACKAGE CHARSET>
      var object expr = read_recursive_no_dot(stream_); # read expression
      dynamic_unbind();
      dynamic_unbind();
      expr = make_references(expr); # unentangle references
      pushSTACK(*stream_); pushSTACK(expr); pushSTACK(S(Kinput));
      funcall(L(set_stream_external_format),3); # (SYS::SET-STREAM-EXTERNAL-FORMAT stream expr :input)
      value1 = NIL; mv_count=0; skipSTACK(3); return; # no values
    }
    # when *READ-SUPPRESS* /= NIL, only read one Object:
    if (test_value(S(read_suppress))) {
      read_recursive_no_dot(stream_); # read Object, and throw away
      value1 = NIL; mv_count=1; skipSTACK(3); return; # NIL as value
    }
    # according to n :
    if (nullp(STACK_0)) {
      # n=NIL -> read Closure:
      var object obj = read_recursive_no_dot(stream_); # read Object
      if (!(consp(obj) && mconsp(Cdr(obj)))) { # length >=2 ?
        pushSTACK(*stream_); # value for Slot STREAM of STREAM-ERROR
        pushSTACK(obj);
        pushSTACK(*stream_); # Stream
        pushSTACK(S(read));
        fehler(stream_error,
               GETTEXT("~ from ~: object #Y~ has not the syntax of a compiled closure")
              );
      }
      skipSTACK(3);
      # execute (SYS::%MAKE-CLOSURE (first obj) (second obj) (cddr obj)):
      pushSTACK(Car(obj)); obj = Cdr(obj); # 1. Argument
      pushSTACK(Car(obj)); obj = Cdr(obj); # 2. Argument
      pushSTACK(obj); # 3. Argument
      funcall(L(make_closure),3);
      mv_count=1; # value1 as value
    } else {
      # n specified -> read Codevector:
      # Syntax: #nY(b1 ... bn), where n is a Fixnum >=0 and b1,...,bn
      # are Fixnums >=0, <256 in Base 16  (with one or two digits).
      # e.g. #9Y(0 4 F CD 6B8FD1e4 5)
      # n is an Integer >=0.
      var uintL n =
        (posfixnump(STACK_0) ? posfixnum_to_L(STACK_0) # Fixnum -> value
                             : bitm(oint_data_len)-1 # Bignum -> big value
        );
      # get new Bit-Vector with n Bytes:
      STACK_1 = allocate_bit_vector(Atype_8Bit,n);
      # stack layout: Stream, Codevektor, n.
      var object ch;
      var uintWL scode;
      # skip Whitespace:
      do {
        read_char_syntax(ch = ,scode = ,stream_); # read character
      } until (!(scode == syntax_whitespace));
      # '(' must follow:
      if (!eq(ch,ascii_char('(')))
        fehler_closure_badchar();
      {
        var uintL index = 0;
        until (index==n) {
          # skip Whitespace:
          do {
            read_char_syntax(ch = ,scode = ,stream_); # read character
          } until (!(scode == syntax_whitespace));
          # Hex-digit must follow:
          var uintB zif = hexziffer(ch,scode);
          # read next Character:
          read_char_syntax(ch = ,scode = ,stream_);
          if (scode == syntax_eof) # EOF -> Error
            fehler_eof_innen(stream_);
          if ((scode == syntax_whitespace) || eq(ch,ascii_char(')'))) {
            # Whitespace or closing parenthese
            # will be pushed back to Stream:
            unread_char(stream_,ch);
          } else {
            # it must be a second Hex-digit
            zif = 16*zif + hexziffer(ch,scode); # add to first Hex-digit
            # (no whitespace is demanded after the second Hex-digit.)
          }
          # zif = read Byte. write into Codevector:
          TheSbvector(STACK_1)->data[index] = zif;
          index++;
        }
      }
      # skip Whitespace:
      do {
        read_char_syntax(ch = ,scode = ,stream_); # read character
      } until (!(scode == syntax_whitespace));
      # ')' must follow:
      if (!eq(ch,ascii_char(')')))
        fehler_closure_badchar();
      #if BIG_ENDIAN_P
      # convert Header from Little-Endian to Big-Endian:
      {
        var Sbvector v = TheSbvector(STACK_1);
        swap(uintB, v->data[CCV_SPDEPTH_1], v->data[CCV_SPDEPTH_1+1]);
        swap(uintB, v->data[CCV_SPDEPTH_JMPBUFSIZE], v->data[CCV_SPDEPTH_JMPBUFSIZE+1]);
        swap(uintB, v->data[CCV_NUMREQ], v->data[CCV_NUMREQ+1]);
        swap(uintB, v->data[CCV_NUMOPT], v->data[CCV_NUMOPT+1]);
        if (v->data[CCV_FLAGS] & bit(7)) {
          swap(uintB, v->data[CCV_NUMKEY], v->data[CCV_NUMKEY+1]);
          swap(uintB, v->data[CCV_KEYCONSTS], v->data[CCV_KEYCONSTS+1]);
        }
      }
      #endif
      # Codevector as value:
      value1 = STACK_1; mv_count=1; skipSTACK(3);
    }
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
LISPFUNN(clisp_pathname_reader,3) # reads #"
  {
    test_no_infix(); # n must be NIL
    # stack layout: Stream, sub-char #\".
    var object string = # read String, that starts with "
      (funcall(L(string_reader),2),value1);
    # when *READ-SUPPRESS* /= NIL, finished immediately:
    if (test_value(S(read_suppress))) {
      value1 = NIL; mv_count=1; return; # NIL as value
    }
    # construct (pathname string) = (values (parse-namestring string)) :
    pushSTACK(string); funcall(L(parse_namestring),1); # (PARSE-NAMESTRING string)
    mv_count=1; # only one value
  }

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
LISPFUNN(ansi_pathname_reader,3) # reads #P
  {
    var object* stream_ = test_no_infix(); # n must be NIL
    var object obj = read_recursive_no_dot(stream_); # read next Object
    # when *READ-SUPPRESS* /= NIL, finished immediately:
    if (test_value(S(read_suppress))) {
      value1 = NIL; mv_count=1; skipSTACK(2); return;
    }
    obj = make_references(obj); # and unentangle references untimely (unnessecary?)
    if (!stringp(obj)) # obj must be a String!
      goto bad;
    # create (pathname obj) = (values (parse-namestring obj)) :
    pushSTACK(obj); funcall(L(parse_namestring),1); # (PARSE-NAMESTRING obj)
    mv_count=1; skipSTACK(2); return; # only one value
   bad:
    pushSTACK(*stream_); # value for Slot STREAM of STREAM-ERROR
    pushSTACK(obj); # Object
    pushSTACK(*stream_); # Stream
    pushSTACK(S(read));
    fehler(stream_error,
           GETTEXT("~ from ~: bad syntax for pathname: #P~")
          );
  }

#ifdef UNIX

# (set-dispatch-macro-character #\# #\!
#   #'(lambda (stream sub-char n)
#       (declare (ignore sub-char))
#       (when n (error ...))
#       (read-line stream)
#       (values)
# )   )
LISPFUNN(unix_executable_reader,3) # reads #!
  {
    var object* stream_ = test_no_infix(); # n must be NIL
    # stack layout: Stream, sub-char #\!.
    loop {
      var object ch = read_char(stream_); # read character
      if (eq(ch,eof_value) || eq(ch,ascii_char(NL)))
        break;
    }
    value1 = NIL; mv_count=0; skipSTACK(2); # return no values
  }

#endif

# ------------------------ LISP-Functions of the Reader -----------------------

# UP: checks an Input-Stream-Argument.
# Default is the value of *STANDARD-INPUT*.
# test_istream(&stream);
# > subr_self: caller (ein SUBR)
# > stream: Input-Stream-Argument
# < stream: Input-Stream (a Stream)
  local void test_istream (object* stream_);
  local void test_istream(stream_)
    var object* stream_;
    {
      var object stream = *stream_;
      if (eq(stream,unbound) || nullp(stream)) {
        # instead of #<UNBOUND> or NIL: value of *STANDARD-INPUT*
        *stream_ = var_stream(S(standard_input),strmflags_rd_ch_B);
      } elif (eq(stream,T)) {
        # instead of T: value of *TERMINAL-IO*
        *stream_ = var_stream(S(terminal_io),strmflags_rd_ch_B);
      } else {
        if (!streamp(stream))
          fehler_stream(stream);
      }
    }

# EOF-Handling, ends Reader-Functions.
# eof_handling()
# > STACK_3: Input-Stream
# > STACK_2: eof-error-p
# > STACK_1: eof-value
# > STACK_0: recursive-p
# < mv_space/mv_count: values
  local Values eof_handling (int mvc);
  local Values eof_handling(mvc)
     var int mvc;
    {
      if (!nullp(STACK_2)) { # eof-error-p /= NIL (e.g. = #<UNBOUND>) ?
        # report Error:
        var object recursive_p = STACK_0;
        if (eq(recursive_p,unbound) || nullp(recursive_p))
          fehler_eof_aussen(&STACK_3); # report EOF
        else
          fehler_eof_innen(&STACK_3); # report EOF within Objekt
      } else {
        # handle EOF:
        var object eofval = STACK_1;
        if (eq(eofval,unbound))
          eofval = NIL; # Default is NIL
        value1 = eofval; mv_count=mvc; skipSTACK(4); # eofval as value
      }
    }

# UP for READ and READ-PRESERVING-WHITESPACE
# read_w(whitespace-p)
# > whitespace-p: indicates, if whitespace has to be consumed afterwards
# > stack layout: input-stream, eof-error-p, eof-value, recursive-p.
# > subr_self: caller (a SUBR) (unnecessary, if input-stream is a Stream)
# < STACK: cleaned up
# < mv_space/mv_count: values
  local Values read_w (object whitespace_p);
  local Values read_w(whitespace_p)
    var object whitespace_p;
    {
      # check input-stream:
      test_istream(&STACK_3);
      # check for recursive-p-Argument:
      var object recursive_p = STACK_0;
      if (eq(recursive_p,unbound) || nullp(recursive_p)) {
        # non-recursive call
        var object obj = read_top(&STACK_3,whitespace_p);
        if (eq(obj,dot_value))
          fehler_dot(STACK_3); # Dot -> Error
        if (eq(obj,eof_value)) {
          return_Values eof_handling(1); # EOF-treatment
        } else {
          value1 = obj; mv_count=1; skipSTACK(4); # obj as value
        }
      } else {
        # recursive call
        value1 = read_recursive_no_dot(&STACK_3); mv_count=1; skipSTACK(4);
      }
    }

LISPFUN(read,0,4,norest,nokey,0,NIL)
# (READ [input-stream [eof-error-p [eof-value [recursive-p]]]]), CLTL p. 375
  {
    return_Values read_w(NIL); # whitespace-p := NIL
  }

LISPFUN(read_preserving_whitespace,0,4,norest,nokey,0,NIL)
# (READ-PRESERVING-WHITESPACE [input-stream [eof-error-p [eof-value [recursive-p]]]]),
# CLTL p. 376
  {
    return_Values read_w(T); # whitespace-p := T
  }

LISPFUN(read_delimited_list,1,2,norest,nokey,0,NIL)
# (READ-DELIMITED-LIST char [input-stream [recursive-p]]), CLTL p. 377
  {
    # check char:
    var object ch = STACK_2;
    if (!charp(ch))
      fehler_char(ch);
    # check input-stream:
    test_istream(&STACK_1);
    # check for recursive-p-Argument:
    var object recursive_p = popSTACK();
    # stack layout: char, input-stream.
    if (eq(recursive_p,unbound) || nullp(recursive_p)) {
      # non-recursive call
      var object* stream_ = &STACK_0;
      # bind SYS::*READ-REFERENCE-TABLE* to empty Table NIL:
      dynamic_bind(S(read_reference_table),NIL);
      # bind SYS::*BACKQUOTE-LEVEL* to NIL:
      dynamic_bind(S(backquote_level),NIL);
      var object obj = read_delimited_list(stream_,ch,eof_value); # read List
      obj = make_references(obj); # unentangle references
      dynamic_unbind();
      dynamic_unbind();
      value1 = obj; # List as value
    } else {
      # recursive call
      value1 = read_delimited_list(&STACK_0,ch,eof_value);
    }
    # (read List both times, no Dotted List allowed.)
    mv_count=1; skipSTACK(2);
  }

LISPFUN(read_line,0,4,norest,nokey,0,NIL)
# (READ-LINE [input-stream [eof-error-p [eof-value [recursive-p]]]]),
# CLTL p. 378
  {
    # check input-stream:
    var object* stream_ = &STACK_3;
    test_istream(stream_);
    get_buffers(); # two empty Buffers on Stack
    if (!read_line(stream_,&STACK_1)) { # read line
      # End of Line
      # copy Buffer and convert into Simple-String:
      value1 = copy_string(STACK_1);
      # free Buffer for reuse:
      O(token_buff_2) = popSTACK(); O(token_buff_1) = popSTACK();
      value2 = NIL; mv_count=2; # NIL as 2. value
      skipSTACK(4); return;
    } else {
      # End of File
      # Buffer empty?
      if (TheIarray(STACK_1)->dims[1] == 0) { # Length (Fill-Pointer) = 0 ?
        # free Buffer for reuse:
        O(token_buff_2) = popSTACK(); O(token_buff_1) = popSTACK();
        # treat EOF specially:
        value2 = T;
        return_Values eof_handling(2);
      } else {
        # copy Buffer and convert into Simple-String:
        value1 = copy_string(STACK_1);
        # free Buffer for reuse:
        O(token_buff_2) = popSTACK(); O(token_buff_1) = popSTACK();
        value2 = T; mv_count=2; # T as 2. value
        skipSTACK(4); return;
      }
    }
  }

LISPFUN(read_char,0,4,norest,nokey,0,NIL)
# (READ-CHAR [input-stream [eof-error-p [eof-value [recursive-p]]]]),
# CLTL p. 379
  {
    # check input-stream:
    var object* stream_ = &STACK_3;
    test_istream(stream_);
    var object ch = read_char(stream_); # read Character
    if (eq(ch,eof_value)) {
      return_Values eof_handling(1);
    } else {
      value1 = ch; mv_count=1; skipSTACK(4); return; # ch as value
    }
  }

LISPFUN(unread_char,1,1,norest,nokey,0,NIL)
# (UNREAD-CHAR char [input-stream]), CLTL p. 379
  {
    # check input-stream:
    var object* stream_ = &STACK_0;
    test_istream(stream_);
    var object ch = STACK_1; # char
    if (!charp(ch)) # must be a character
      fehler_char(ch);
    unread_char(stream_,ch); # push back char to Stream
    value1 = NIL; mv_count=1; skipSTACK(2); # NIL als Wert
  }

LISPFUN(peek_char,0,5,norest,nokey,0,NIL)
# (PEEK-CHAR [peek-type [input-stream [eof-error-p [eof-value [recursive-p]]]]]),
# CLTL p. 379
  {
    # check input-stream:
    var object* stream_ = &STACK_3;
    test_istream(stream_);
    # distinction of cases by peek-type:
    var object peek_type = STACK_4;
    if (eq(peek_type,unbound) || nullp(peek_type)) {
      # Default NIL: peek one character
      var object ch = peek_char(stream_);
      if (eq(ch,eof_value))
        goto eof;
      value1 = ch; mv_count=1; skipSTACK(5); return; # ch as value
    } elif (eq(peek_type,T)) {
      # T: Whitespace-Peek
      var object ch = wpeek_char_eof(stream_);
      if (eq(ch,eof_value))
        goto eof;
      value1 = ch; mv_count=1; skipSTACK(5); return; # ch as value
    } elif (charp(peek_type)) {
      # peek-type is a Character
      var object ch;
      loop {
        ch = read_char(stream_); # read character
        if (eq(ch,eof_value))
          goto eof;
        if (eq(ch,peek_type)) # the preset End-character?
          break;
      }
      unread_char(stream_,ch); # push back character
      value1 = ch; mv_count=1; skipSTACK(5); return; # ch as value
    } else {
      pushSTACK(peek_type);        # TYPE-ERROR slot DATUM
      pushSTACK(O(type_peektype)); # TYPE-ERROR slot EXPECTED-TYPE
      pushSTACK(peek_type);
      pushSTACK(TheSubr(subr_self)->name);
      fehler(type_error,
             GETTEXT("~: peek type should be NIL or T or a character, not ~")
            );
    }
   eof: # EOF
    eof_handling(1); skipSTACK(1); return;
  }

LISPFUN(listen,0,1,norest,nokey,0,NIL)
# (LISTEN [input-stream]), CLTL p. 380
  {
    test_istream(&STACK_0); # check input-stream
    if (ls_avail_p(listen_char(popSTACK()))) {
      value1 = T; mv_count=1; # value T
    } else {
      value1 = NIL; mv_count=1; # value NIL
    }
  }

LISPFUNN(read_char_will_hang_p,1)
# (READ-CHAR-WILL-HANG-P input-stream)
# tests whether READ-CHAR-NO-HANG will return immediately without reading a
# character, but accomplishes this without actually calling READ-CHAR-NO-HANG,
# thus avoiding the need for UNREAD-CHAR and preventing side effects.
  {
    test_istream(&STACK_0); # check input-stream
    value1 = (ls_wait_p(listen_char(popSTACK())) ? T : NIL); mv_count=1;
  }

LISPFUN(read_char_no_hang,0,4,norest,nokey,0,NIL)
# (READ-CHAR-NO-HANG [input-stream [eof-error-p [eof-value [recursive-p]]]]),
# CLTL p. 380
  {
    # check input-stream:
    var object* stream_ = &STACK_3;
    test_istream(stream_);
    var object stream = *stream_;
    if (builtin_stream_p(stream)
        ? !(TheStream(stream)->strmflags & bit(strmflags_rd_ch_bit_B))
        : !instanceof(stream,O(class_fundamental_input_stream))
       )
      fehler_illegal_streamop(S(read_char_no_hang),stream);
    var signean status = listen_char(stream);
    if (ls_eof_p(status)) { # EOF ?
      return_Values eof_handling(1);
    } elif (ls_avail_p(status)) { # character available
      var object ch = read_char(stream_); # read Character
      if (eq(ch,eof_value)) { # query for EOF, for safety reasons
        return_Values eof_handling(1);
      } else {
        value1 = ch; mv_count=1; skipSTACK(4); return; # ch as value
      }
    } else { # ls_wait_p(status) # no character available
      # instead of waiting, return NIL as Wert, immediately:
      value1 = NIL; mv_count=1; skipSTACK(4); return;
    }
  }

LISPFUN(clear_input,0,1,norest,nokey,0,NIL)
# (CLEAR-INPUT [input-stream]), CLTL p. 380
  {
    test_istream(&STACK_0); # check input-stream
    clear_input(popSTACK());
    value1 = NIL; mv_count=1; # value NIL
  }

LISPFUN(read_from_string,1,2,norest,key,3,\
        (kw(preserve_whitespace),kw(start),kw(end)) )
# (READ-FROM-STRING string [eof-error-p [eof-value [:preserve-whitespace]
#                   [:start] [:end]]]),
# CLTL p. 380
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
  {
    # stack layout: string, eof-error-p, eof-value, preserve-whitespace, start, end.
    # process :preserve-whitespace-Argument:
    var object preserve_whitespace = STACK_2;
    if (eq(preserve_whitespace,unbound))
      preserve_whitespace = NIL;
    # call MAKE-STRING-INPUT-STREAM with Arguments string, start, end:
    STACK_2 = STACK_5; # string
    if (eq(STACK_1,unbound))
      STACK_1 = Fixnum_0; # start has Default 0
    if (eq(STACK_0,unbound))
      STACK_0 = NIL; # end has Default NIL
    STACK_5 = preserve_whitespace;
    funcall(L(make_string_input_stream),3);
    # stack layout: preserve-whitespace, eof-error-p, eof-value.
    pushSTACK(STACK_1); pushSTACK(STACK_1);
    STACK_3 = STACK_2 = value1;
    # stack layout: preserve-whitespace, stream, stream, eof-error-p, eof-value.
    pushSTACK(NIL); read_w(STACK_5); # READ respectively READ-PRESERVE-WHITESPACE
    # stack layout: preserve-whitespace, stream.
    STACK_1 = value1; # read Objekt
    funcall(L(string_input_stream_index),1); # (SYS::STRING-INPUT-STREAM-INDEX stream)
    value2 = value1; value1 = popSTACK(); # Index as 2., Objekt as 1. value
    mv_count=2;
  }

LISPFUN(parse_integer,1,0,norest,key,4,\
        (kw(start),kw(end),kw(radix),kw(junk_allowed)) )
# (PARSE-INTEGER string [:start] [:end] [:radix] [:junk-allowed]), CLTL p. 381
  {
    # process :junk-allowed-Argument:
    var bool junk_allowed;
    {
      var object arg = popSTACK();
      if (eq(arg,unbound) || nullp(arg))
        junk_allowed = false;
      else
        junk_allowed = true;
    }
    # junk_allowed = value of :junk-allowed-Argument.
    # process :radix-Argument:
    var uintL base;
    {
      var object arg = popSTACK();
      if (eq(arg,unbound)) {
        base = 10; # Default 10
      } else {
        if (posfixnump(arg) &&
            (base = posfixnum_to_L(arg), ((base >= 2) && (base <= 36)))
           ) {
          # OK
        } else {
          pushSTACK(arg);           # TYPE-ERROR slot DATUM
          pushSTACK(O(type_radix)); # TYPE-ERROR slot EXPECTED-TYPE
          pushSTACK(arg); # base
          pushSTACK(S(Kradix));
          pushSTACK(TheSubr(subr_self)->name);
          fehler(type_error,
                 GETTEXT("~: ~ argument should be an integer between 2 and 36, not ~")
                );
        }
      }
    }
    # base = value of :radix-Arguments.
    # check string, :start and :end:
    var stringarg arg;
    var object string = test_string_limits_ro(&arg);
    # STACK is not cleared up.
    var uintL start = arg.index; # value of :start-Arguments
    var uintL len = arg.len; # number of the addressed characters
    var const chart* charptr;
    unpack_sstring_alloca(arg.string,arg.len,arg.offset+arg.index, charptr=);
    # loop variables:
    var uintL index = start;
    var uintL count = len;
    var uintL start_offset;
    var uintL end_offset;
    # and now:
    #   string : the string,
    #   arg.string : its data-vector (a simple-string),
    #   start : index of the first character in the string
    #   charptr : pointer in the data-vector of the next character,
    #   index : index in the string,
    #   count : the number of remaining characters.
    var signean sign;
    {
      var chart c; # the last character read
      # step 1: skip whitespace
      loop {
        if (count==0) # the string has already ended?
          goto badsyntax;
        c = *charptr; # the next character
        if (!(orig_syntax_table_get(c) == syntax_whitespace)) # no whitespace?
          break;
        charptr++; index++; count--; # skip whitespace
      }
      # step 2: read the sign
      sign = 0; # sign := positive
      switch (as_cint(c)) {
        case '-': sign = -1; # sign := negative
        case '+': # sign found
          charptr++; index++; count--; # skip
          if (count==0) # the string has already ended?
            goto badsyntax;
        default: break;
      }
    }
    # done with sign, still should be (count>0).
    start_offset = arg.offset + index;
    # now:  start_offset = offset of the first digit in the data vector
    # step 3: read digits
    loop {
      var cint c = as_cint(*charptr); # the next character
      # check the digits: (digit-char-p (code-char c) base) ?
      # (cf. DIGIT-CHAR-P in CHARSTRG.D)
      if (c > 'z') break; # too large -> no
      if (c >= 'a') { c -= 'a'-'A'; } # upcase 'a'<= char <='z'
      # now $00 <= c <= $60.
      if (c < '0') break;
      # $30 <= c <= $60 convert to the numeric value
      if (c <= '9')
        c = c - '0';
      elif (c >= 'A')
        c = c - 'A' + 10;
      else
        break;
      # now 0 =< c <=41 is the numeric value of the digit
      if (c >= (uintB)base) # only valid if 0 <= c < base.
        break;
      # *charptr is a valid digit.
      charptr++; index++; count--; # skip
      if (count==0)
        break;
    }
    # done with the digit.
    end_offset = arg.offset + index;
    # now:  end_offset = offset after the last digit in the data-vector.
    if (start_offset == end_offset) # there were no digits?
      goto badsyntax;
    # step 4: skip the final whitespace
    if (!junk_allowed) { # if junk_allowed, nothing is to be done
      while (!(count==0)) {
        var chart c = *charptr; # the next character
        if (!(orig_syntax_table_get(c) == syntax_whitespace)) # no whitespace?
          goto badsyntax;
        charptr++; index++; count--; # skip whitespace
      }
    }
    # step 5: convert the sequence of digits into a number
    value1 = read_integer(base,sign,arg.string,start_offset,end_offset);
    value2 = fixnum(index);
    mv_count=2; return;
   badsyntax: # illegale character
    if (!junk_allowed) { # signal an error
      pushSTACK(unbound); # STREAM-ERROR slot STREAM
      pushSTACK(string);
      pushSTACK(TheSubr(subr_self)->name);
      fehler(stream_error,
             GETTEXT("~: string ~ does not have integer syntax")
            );
    }
    value1 = NIL;
    value2 = fixnum(index);
    mv_count=2; return;
  }


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
    {
      var uintB ziffern[10]; # max. 10 Ziffern, da 0 <= x < 2^32 <= 10^10
      var uintB* ziffptr = &ziffern[0];
      var uintC ziffcount = 0; # Anzahl der Ziffern
      # Ziffern produzieren:
      do {
        var uintB zif;
        divu_3216_3216(x,10,x=,zif=); # x := floor(x/10), zif := Rest
        *ziffptr++ = zif; ziffcount++; # Ziffer abspeichern
      } until (x==0);
      # Ziffern in umgekehrter Reihenfolge ausgeben:
      dotimespC(ziffcount,ziffcount, {
        write_ascii_char(stream_,'0' + *--ziffptr);
      });
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
    {
      write_ascii_char(stream_, ( x<10 ? '0'+(uintB)x : 'A'+(uintB)x-10 ) );
    }

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
    {
      pr_hex1(stream_,(uint4)(x>>4)); # Bits 7..4 ausgeben
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
    {
      var oint x = (as_oint(obj) >> oint_addr_shift) << addr_shift;
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
    {
      write_ascii_char(stream_,'#'); write_ascii_char(stream_,'x'); # Präfix für "Hexadezimal"
      var sintC k = (sizeof(uintP)-1)*8;
      do {
        pr_hex2(stream_,(uint8)(x >> k));
      } while ((k -= 8) >= 0);
    }
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
    {
      dynamic_bind(S(print_readably),NIL); # bind *PRINT-READABLY* to NIL
      pushSTACK(obj); # PRINT-NOT-READABLE slot OBJECT
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
    {
      var object print_case = S(print_case);
      pushSTACK(Symbol_value(print_case)); # TYPE-ERROR slot DATUM
      pushSTACK(O(type_printcase));        # TYPE-ERROR slot EXPECTED-TYPE
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
    {
      if (len==0) return;
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
    {
      write_sstring_ab(stream_,string,0,Sstring_length(string));
    }

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
    {
      if (simple_string_p(string)) {
        # Simple-String
        write_sstring(stream_,string);
      } else {
        # nicht-simpler String
        var uintL len = vector_length(string); # Länge
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
    {
      # (READTABLE-CASE *READTABLE*) abfragen:
      var object readtable;
      get_readtable(readtable = ); # aktuelle Readtable
      switch ((uintW)posfixnum_to_L(TheReadtable(readtable)->readtable_case)) {
        case case_upcase:
          # *PRINT-CASE* abfragen. Danach richtet sich, wie Großbuchstaben
          # ausgegeben werden. Kleinbuchstaben werden immer klein ausgegeben.
          switch_print_case(
            # :UPCASE -> Großbuchstaben in Upcase ausgeben:
            {
              write_sstring(stream_,string);
            },
            # :DOWNCASE -> Großbuchstaben in Downcase ausgeben:
            do_downcase:
            {
              var uintL count = Sstring_length(string);
              if (count > 0) {
                var uintL index = 0;
                pushSTACK(string); # Simple-String retten
                SstringDispatch(string,
                  { dotimespL(count,count, {
                      write_code_char(stream_,down_case(TheSstring(STACK_0)->data[index]));
                      index++;
                    });
                  },
                  { dotimespL(count,count, {
                      write_code_char(stream_,down_case(as_chart(TheSmallSstring(STACK_0)->data[index])));
                      index++;
                    });
                  }
                  );
                skipSTACK(1);
              }
            },
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
            {
              var uintL count = Sstring_length(string);
              if (count > 0) {
                var bool flag = false;
                var uintL index = 0;
                pushSTACK(string); # Simple-String retten
                SstringDispatch(string,
                  { dotimespL(count,count, {
                      # flag zeigt an, ob gerade innerhalb eines Wortes
                      var bool oldflag = flag;
                      var chart c = TheSstring(STACK_0)->data[index]; # nächstes Zeichen
                      if ((flag = alphanumericp(c)) && oldflag)
                        # alphanumerisches Zeichen im Wort:
                        c = down_case(c); # Groß- in Kleinbuchstaben umwandeln
                      write_code_char(stream_,c); # und ausgeben
                      index++;
                    });
                  },
                  { dotimespL(count,count, {
                      # flag zeigt an, ob gerade innerhalb eines Wortes
                      var bool oldflag = flag;
                      var chart c = as_chart(TheSmallSstring(STACK_0)->data[index]); # nächstes Zeichen
                      if ((flag = alphanumericp(c)) && oldflag)
                        # alphanumerisches Zeichen im Wort:
                        c = down_case(c); # Groß- in Kleinbuchstaben umwandeln
                      write_code_char(stream_,c); # und ausgeben
                      index++;
                    });
                  }
                  );
                skipSTACK(1);
              }
            }
            );
          break;
        case case_downcase:
          # *PRINT-CASE* abfragen. Danach richtet sich, wie Kleinbuchstaben
          # ausgegeben werden. Großbuchstaben werden immer groß ausgegeben.
          switch_print_case(
            # :UPCASE -> Kleinbuchstaben in Upcase ausgeben:
            do_upcase:
            {
              var uintL count = Sstring_length(string);
              if (count > 0) {
                var uintL index = 0;
                pushSTACK(string); # Simple-String retten
                SstringDispatch(string,
                  { dotimespL(count,count, {
                      write_code_char(stream_,up_case(TheSstring(STACK_0)->data[index]));
                      index++;
                    });
                  },
                  { dotimespL(count,count, {
                      write_code_char(stream_,up_case(as_chart(TheSmallSstring(STACK_0)->data[index])));
                      index++;
                    });
                  }
                  );
                skipSTACK(1);
              }
            },
            # :DOWNCASE -> Kleinbuchstaben in Downcase ausgeben:
            {
              write_sstring(stream_,string);
            },
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
            {
              var uintL count = Sstring_length(string);
              if (count > 0) {
                var bool flag = false;
                var uintL index = 0;
                pushSTACK(string); # Simple-String retten
                SstringDispatch(string,
                  { dotimespL(count,count, {
                      # flag zeigt an, ob gerade innerhalb eines Wortes
                      var bool oldflag = flag;
                      var chart c = TheSstring(STACK_0)->data[index]; # nächstes Zeichen
                      if ((flag = alphanumericp(c)) && !oldflag)
                        # alphanumerisches Zeichen am Wortanfang:
                        c = up_case(c); # Klein- in Großbuchstaben umwandeln
                      write_code_char(stream_,c); # und ausgeben
                      index++;
                    });
                  },
                  { dotimespL(count,count, {
                      # flag zeigt an, ob gerade innerhalb eines Wortes
                      var bool oldflag = flag;
                      var chart c = as_chart(TheSmallSstring(STACK_0)->data[index]); # nächstes Zeichen
                      if ((flag = alphanumericp(c)) && !oldflag)
                        # alphanumerisches Zeichen am Wortanfang:
                        c = up_case(c); # Klein- in Großbuchstaben umwandeln
                      write_code_char(stream_,c); # und ausgeben
                      index++;
                    });
                  }
                  );
                skipSTACK(1);
              }
            }
            );
          break;
        case case_preserve:
          # *PRINT-CASE* ignorieren.
          write_sstring(stream_,string);
          break;
        case case_invert:
          # *PRINT-CASE* ignorieren.
          {
            var bool seen_uppercase = false;
            var bool seen_lowercase = false;
            var uintL count = Sstring_length(string);
            if (count > 0) {
              SstringDispatch(string,
                {
                  var const chart* cptr = &TheSstring(string)->data[0];
                  dotimespL(count,count, {
                    var chart c = *cptr++;
                    if (!chareq(c,up_case(c)))
                      seen_lowercase = true;
                    if (!chareq(c,down_case(c)))
                      seen_uppercase = true;
                  });
                },
                {
                  var const scint* cptr = &TheSmallSstring(string)->data[0];
                  dotimespL(count,count, {
                    var chart c = as_chart(*cptr++);
                    if (!chareq(c,up_case(c)))
                      seen_lowercase = true;
                    if (!chareq(c,down_case(c)))
                      seen_uppercase = true;
                  });
                }
                );
            }
            if (seen_uppercase) {
              if (!seen_lowercase)
                goto do_downcase;
            } else {
              if (seen_lowercase)
                goto do_upcase;
            }
            write_sstring(stream_,string);
          }
          break;
        default: NOTREACHED
      }
    }

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
    {
      var uintL count;
      dotimesL(count,posfixnum_to_L(anzahl), {
        write_ascii_char(stream_,' ');
      });
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
  {
    var object prm = Symbol_value(S(print_right_margin));
    if (nullp(prm))
      return Symbol_value(S(prin_linelength));
    elif (posfixnump(prm))
      return prm;
    elif (posbignump(prm))
      return fixnum(bit(oint_data_len)-1);
    else {
      pushSTACK(prm); pushSTACK(S(print_right_margin));
      fehler(error,
             GETTEXT("~: must be a positive integer or NIL, not ~")
            );
    }
  }

# Returns the string-width of a PPHELP stream block.
  local uintL pphelp_string_width (object string);
  local uintL pphelp_string_width(string)
    var object string;
    {
      var uintL width = 0;
      var uintL len = TheIarray(string)->dims[1]; # length = fill-pointer
      if (len > 0) {
        string = TheIarray(string)->data; # mutable simple-string
        var const chart* charptr = &TheSstring(string)->data[0];
        dotimespL(len,len, {
          width += char_width(*charptr); charptr++;
        });
      }
      return width;
    }

# UP: Fängt in PPHELP-Stream A5 eine neue Zeile an.
# pphelp_newline(&stream);
# > stream: Stream
# < stream: Stream
# can trigger GC
  local void pphelp_newline (const object* stream_);
  local void pphelp_newline(stream_)
    var const object* stream_;
    {
      # (push (make-ssstring 50) (strm-pphelp-strings stream)) :
      pushSTACK(make_ssstring(50)); # neuer Semi-Simple-String
      var object new_cons = allocate_cons(); # neues Cons
      Car(new_cons) = popSTACK();
      var object stream = *stream_;
      Cdr(new_cons) = TheStream(stream)->strm_pphelp_strings;
      TheStream(stream)->strm_pphelp_strings = new_cons;
      # Line-Position := 0, Modus := Mehrzeiler :
      TheStream(stream)->strm_pphelp_lpos = Fixnum_0;
      TheStream(stream)->strm_pphelp_modus = mehrzeiler;
    }

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
    {
      var object stream = *stream_;
      if (!(builtin_stream_p(stream)
            && (TheStream(stream)->strmtype == strmtype_pphelp)
         ) ) {
        # normaler Stream
        write_ascii_char(stream_,'(');
      } else {
        # Pretty-Print-Hilfs-Stream
        var object pos = # Position für die Klammer zu
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
    {
      var object stream = *stream_;
      if (!(builtin_stream_p(stream)
            && (TheStream(stream)->strmtype == strmtype_pphelp)
         ) ) {
        # normaler Stream
        write_ascii_char(stream_,')');
      } else {
        # Pretty-Print-Hilfs-Stream
        # gewünschte Position der Klammer zu holen:
        var object pos = Symbol_value(S(prin_rpar)); # SYS::*PRIN-RPAR*
        if (nullp(pos)) goto hinten; # keine -> Klammer hinten ausgeben
        # Klammer an Position pos ausgeben:
        if (eq(TheStream(stream)->strm_pphelp_modus,mehrzeiler)
            && !nullp(Cdr(TheStream(stream)->strm_pphelp_strings))
           ) {
          # Mehrzeiler mit mehr als einer Zeile ("echter" Mehrzeiler)
          # Klammer an die gewünschte Position ausgeben.
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
          var chart* charptr = &TheSstring(lastline)->data[0];
          # Teste, ob need Spaces kommen:
          {
            var uintL count;
            dotimespL(count,need, {
              if (!chareq(*charptr++,ascii(' '))) # Space ?
                goto new_line; # nein -> neue Zeile anfangen
            });
          }
          var chart* charptr1 = charptr; # Position merken
          # Teste, ob len-need mal Space oder ')' kommt:
          {
            var uintL count;
            dotimesL(count,len-need, {
              var chart c = *charptr++;
              if (!(chareq(c,ascii(' ')) || chareq(c,ascii(')')))) # Space oder ')' ?
                goto new_line; # nein -> neue Zeile anfangen
            });
          }
          # Klammer an die gewünschte Position pos = need-1 setzen:
          *--charptr1 = ascii(')');
        } else {
          # Einzeiler.
          # Klammer muss wohl hinten ausgegeben werden.
          # Ausnahme: Wenn Line-Position = SYS::*PRIN-LINELENGTH* ist,
          #           würde über die Zeile hinausgeschrieben;
          #           stattdessen wird eine neue Zeile angefangen.
          # Max Right Margin == Line-Position ?
          if (eq(right_margin(),TheStream(stream)->strm_pphelp_lpos)) {
           new_line: # neue Zeile anfangen
            pphelp_newline(stream_); spaces(stream_,pos);
          }
         hinten: # Klammer hinten ausgeben
          write_ascii_char(stream_,')');
        }
        # Bindung von SYS::*PRIN-RPAR* auflösen:
        dynamic_unbind();
      }
    }

# Justify
# -------
# Korrekt zu schachteln,
# jeweils 1 mal JUSTIFY_START,
# dann beliebige Ausgaben, durch JUSTIFY_SPACE getrennt,
# dann 1 mal entweder
#     JUSTIFY_END_ENG (fasst auch in Mehrzeilern kurze Blöcke in eine Zeile)
#     oder
#     JUSTIFY_END_WEIT (in Mehrzeilern belegt jeder Block eine eigene Zeile).
  #define JUSTIFY_START(n)  justify_start(stream_,n);
  #define JUSTIFY_SPACE     justify_space(stream_);
  #define JUSTIFY_END_ENG   justify_end_eng(stream_);
  #define JUSTIFY_END_WEIT  justify_end_weit(stream_);

# SYS::*PRIN-TRAILLENGTH* = number of columns that need to be reserved for
#                           closing parentheses on the current line; bound
#                           to 0 for all objects immediately followed by
#                           JUSTIFY_SPACE. Used only if *PRINT-RPARS* = NIL.
# Preparation of an item to be justified.
  #define JUSTIFY_LAST(is_last)  \
    { if (is_last) justify_last(); }

# UP: Leert einen Pretty-Print-Hilfsstream.
# justify_empty_1(&stream);
# > stream: Stream
# < stream: Stream
# can trigger GC
  local void justify_empty_1 (const object* stream_);
  local void justify_empty_1(stream_)
    var const object* stream_;
    {
      pushSTACK(make_ssstring(50)); # neuer Semi-Simple-String
      var object new_cons = allocate_cons(); # neues Cons
      Car(new_cons) = popSTACK();
      # new_cons = (list (make-ssstring 50))
      var object stream = *stream_;
      TheStream(stream)->strm_pphelp_strings = new_cons; # neue, leere Zeile
      TheStream(stream)->strm_pphelp_modus = einzeiler; # Modus := Einzeiler
    }

# UP: Beginnt einen Justify-Block.
# justify_start(&stream,traillength);
# > stream: Stream
# > traillength: additional width that needs to be reserved for closing brackets on this level
# < stream: Stream
# verändert STACK
  local void justify_start (const object* stream_, uintL traillength);
  local void justify_start(stream_,traillength)
    var const object* stream_;
    var uintL traillength;
    {
      var object stream = *stream_;
      # Bind SYS::*PRIN-TRAILLENGTH* to 0 and save its previous value,
      # incremented by traillength, in SYS::*PRIN-PREV-TRAILLENGTH*.
      dynamic_bind(S(prin_prev_traillength),fixnum_inc(Symbol_value(S(prin_traillength)),traillength));
      dynamic_bind(S(prin_traillength),Fixnum_0);
      if (!(builtin_stream_p(stream)
            && (TheStream(stream)->strmtype == strmtype_pphelp)
         ) ) {
        # normaler Stream -> nichts zu tun
      } else {
        # Pretty-Print-Hilfs-Stream
        # SYS::*PRIN-JBSTRINGS* an den Inhalt des Streams binden:
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
    {
      var object stream = *stream_;
      var object new_cons;
      # SYS::*PRIN-JBLOCKS* um den Inhalt des Streams erweitern:
      if (eq(TheStream(stream)->strm_pphelp_modus,mehrzeiler)) {
        # Mehrzeiler.
        # (push strings SYS::*PRIN-JBLOCKS*)
        new_cons = allocate_cons(); # neues Cons
        Car(new_cons) = TheStream(*stream_)->strm_pphelp_strings;
      } else {
        # Einzeiler.
        # (push (first strings) SYS::*PRIN-JBLOCKS*), oder kürzer:
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
    {
      if (!(builtin_stream_p(*stream_)
            && (TheStream(*stream_)->strmtype == strmtype_pphelp)
         ) ) {
        # normaler Stream -> nur ein Space
        write_ascii_char(stream_,' ');
      } else {
        # Pretty-Print-Hilfs-Stream
        justify_empty_2(stream_); # Streaminhalt retten
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
    {
      if (!(builtin_stream_p(*stream_)
            && (TheStream(*stream_)->strmtype == strmtype_pphelp)
         ) ) {
        # normaler Stream -> nichts zu tun
      } else {
        # Pretty-Print-Hilfs-Stream
        justify_empty_2(stream_); # Streaminhalt retten
        # Streaminhalt restaurieren, d.h. Werte von SYS::*PRIN-JBSTRINGS*,
        # SYS::*PRIN-JBMODUS*, SYS::*PRIN-JBLPOS* in den Stream zurück:
        var object stream = *stream_;
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
        loop { # Blockliste STACK_0 durchlaufen:
          var object block = Car(STACK_0); # nächster Block
          STACK_0 = Cdr(STACK_0); # Blockliste verkürzen
          if (consp(block)) {
            # Mehrzeiliger Teilblock
            # Zeilen in die richtige Reihenfolge bringen:
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
            if (matomp(STACK_0)) { # Restliste leer?
              # ja -> Line-Position zurück, fertig
              TheStream(stream)->strm_pphelp_lpos = STACK_1;
              break;
            }
            # neue Zeile anfangen und weiter:
            goto new_line;
          } else {
            # Einzeiliger Teilblock
            # auf den PPHELP-Stream ausgeben:
            write_string(stream_,block);
            if (matomp(STACK_0)) # Restliste leer?
              break; # ja -> fertig
            # nächster Block ein Mehrzeiler?
            block = Car(STACK_0); # nächster Block
            if (atomp(block)) { # ein Mehrzeiler oder Einzeiler?
              # Es ist ein Einzeiler.
              # Passt er noch auf dieselbe Zeile, d.h. ist
              # Line-Position + 1 + string_width(Einzeiler) + Traillength <= L ?
              var object linelength = right_margin();
              if (nullp(linelength) # =NIL -> passt
                  || (posfixnum_to_L(TheStream(*stream_)->strm_pphelp_lpos) # Line-Position
                      + pphelp_string_width(block) # Breite des Einzeilers
                      + (nullp(Symbol_value(S(print_rpars))) && matomp(Cdr(STACK_0)) ? posfixnum_to_L(Symbol_value(S(prin_prev_traillength))) : 0) # SYS::*PRIN-PREV-TRAILLENGTH*
                      < posfixnum_to_L(linelength) # < linelength ?
                 )   ) {
                # Passt noch.
                # Space statt Newline ausgeben:
                write_ascii_char(stream_,' ');
              } else {
                # Passt nicht mehr.
                goto new_line;
              }
            } else {
              # Mehrzeiler -> neue Zeile und weiter
             new_line: # neue Zeile anfangen
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
      }
      # Bindungen von JUSTIFY_START rückgängig machen:
      dynamic_unbind();
      dynamic_unbind();
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
    {
      if (!(builtin_stream_p(*stream_)
            && (TheStream(*stream_)->strmtype == strmtype_pphelp)
         ) ) {
        # normaler Stream -> nichts zu tun
      } else {
        # Pretty-Print-Hilfs-Stream
        justify_empty_2(stream_); # Streaminhalt retten
        # Streaminhalt restaurieren, d.h. Werte von SYS::*PRIN-JBSTRINGS*,
        # SYS::*PRIN-JBMODUS*, SYS::*PRIN-JBLPOS* in den Stream zurück:
        var object stream = *stream_;
        # jetzige Line-Position retten:
        pushSTACK(TheStream(stream)->strm_pphelp_lpos);
        # alten Streaminhalt wiederherstellen:
        TheStream(stream)->strm_pphelp_strings = Symbol_value(S(prin_jbstrings));
        TheStream(stream)->strm_pphelp_modus = Symbol_value(S(prin_jbmodus));
        TheStream(stream)->strm_pphelp_lpos = Symbol_value(S(prin_jblpos));
        # Prüfe, ob die Blöcke in SYS::*PRIN-JBLOCKS* alle Einzeiler sind:
        {
          var object blocks = Symbol_value(S(prin_jblocks)); # SYS::*PRIN-JBLOCKS*
          do { # (nichtleere) Blockliste durchgehen:
            if (mconsp(Car(blocks))) # ein Teilblock Mehrzeiler ?
              goto gesamt_mehrzeiler; # ja -> insgesamt ein Mehrzeiler
            blocks = Cdr(blocks);
          } while (consp(blocks));
        }
        # Prüfe, ob die Blöcke in SYS::*PRIN-JBLOCKS*
        # (jeder Block Einzeiler) zusammen einen Einzeiler ergeben können:
        # Ist L=NIL (keine Randbeschränkung) oder
        # L1 + (Gesamtbreite der Blöcke) + (Anzahl der Blöcke-1) + Traillength <= L ?
        {
          var object linelength = right_margin();
          if (nullp(linelength)) goto gesamt_einzeiler; # =NIL -> Einzeiler
          var uintL totalneed = posfixnum_to_L(Symbol_value(S(prin_l1))); # Summe := L1 = SYS::*PRIN-L1*
          var object blocks = Symbol_value(S(prin_jblocks)); # SYS::*PRIN-JBLOCKS*
          do { # (nichtleere) Blockliste durchgehen:
            var object block = Car(blocks); # Block (Einzeiler)
            totalneed += pphelp_string_width(block) + 1; # dessen Breite+1 dazu
            blocks = Cdr(blocks);
          } while (consp(blocks));
          if (nullp(Symbol_value(S(print_rpars))))
            totalneed += posfixnum_to_L(Symbol_value(S(prin_prev_traillength))); # SYS::*PRIN-PREV-TRAILLENGTH*
          # totalneed = L1 + (Gesamtbreite der Blöcke) + (Anzahl der Blöcke) + Traillength
          # Vergleiche dies mit linelength + 1 :
          if (totalneed <= posfixnum_to_L(linelength)+1)
            goto gesamt_einzeiler;
          else
            goto gesamt_mehrzeiler;
        }
       gesamt_einzeiler: # Insgesamt ein Einzeiler.
        # Blöcke einzeln, durch Spaces getrennt, auf den Stream ausgeben:
        pushSTACK(nreverse(Symbol_value(S(prin_jblocks)))); # (nreverse SYS::*PRIN-JBLOCKS*)
        loop { # (nichtleere) Blockliste STACK_0 durchlaufen:
          var object block = Car(STACK_0); # nächster Block
          # (ein Einzeiler, String ohne #\Newline)
          STACK_0 = Cdr(STACK_0); # Blockliste verkürzen
          write_string(stream_,block); # Block auf den Stream ausgeben
          if (matomp(STACK_0)) # Restliste leer -> fertig
            break;
          write_ascii_char(stream_,' '); # #\Space ausgeben
        }
        goto fertig;
       gesamt_mehrzeiler: # Insgesamt ein Mehrzeiler.
        # Blöcke einzeln, durch Newline getrennt, auf den Stream ausgeben:
        pushSTACK(nreverse(Symbol_value(S(prin_jblocks)))); # (nreverse SYS::*PRIN-JBLOCKS*)
        loop { # (nichtleere) Blockliste STACK_0 durchlaufen:
          var object block = Car(STACK_0); # nächster Block
          STACK_0 = Cdr(STACK_0); # Blockliste verkürzen
          if (consp(block)) {
            # Mehrzeiliger Teilblock
            # Zeilen in die richtige Reihenfolge bringen:
            block = nreverse(block);
            # erste Zeile auf den PPHELP-Stream ausgeben:
            pushSTACK(block);
            write_string(stream_,Car(block));
            block = popSTACK();
            # restliche Zeilen an die Zeilen im Stream vorne dranhängen:
            stream = *stream_;
            TheStream(stream)->strm_pphelp_strings =
              nreconc(Cdr(block),TheStream(stream)->strm_pphelp_strings);
          } else {
            # Einzeiliger Teilblock
            # auf den PPHELP-Stream ausgeben:
            write_string(stream_,block);
          }
          if (matomp(STACK_0)) # Restliste leer?
            break;
          pphelp_newline(stream_); # neue Zeile anfangen
          spaces(stream_,Symbol_value(S(prin_lm))); # SYS::*PRIN-LM* Spaces
        }
        stream = *stream_;
        # Line-Position zurück:
        TheStream(stream)->strm_pphelp_lpos = STACK_1;
        # GesamtModus := Mehrzeiler:
        TheStream(stream)->strm_pphelp_modus = mehrzeiler;
        goto fertig;
       fertig: # Line-Position stimmt nun.
        skipSTACK(2); # leere Restliste und alte Line-Position vergessen
        # Bindungen von JUSTIFY_START rückgängig machen:
        dynamic_unbind();
        dynamic_unbind();
        dynamic_unbind();
        dynamic_unbind();
      }
      # Bindungen von JUSTIFY_START rückgängig machen:
      dynamic_unbind();
      dynamic_unbind();
    }

# Prepares the justification of the last item in a sequence of JUSTIFY_SPACE
# separated items.
# justify_last();
  local void justify_last (void);
  local void justify_last()
    {
      # SYS::*PRIN-TRAILLENGTH* := SYS::*PRIN-PREV-TRAILLENGTH*
      Symbol_value(S(prin_traillength)) = Symbol_value(S(prin_prev_traillength));
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
    {
      if (!(builtin_stream_p(*stream_)
            && (TheStream(*stream_)->strmtype == strmtype_pphelp)
         ) ) {
        # normaler Stream -> nichts zu tun
      } else {
        # Pretty-Print-Hilfs-Stream
        # SYS::*PRIN-L1* binden:
        {
          var object new_L1 = fixnum_inc(Symbol_value(S(prin_l1)),delta);
          dynamic_bind(S(prin_l1),new_L1);
        }
        # SYS::*PRIN-LM* binden:
        {
          var object new_LM = fixnum_inc(Symbol_value(S(prin_lm)),delta);
          dynamic_bind(S(prin_lm),new_LM);
        }
      }
    }

# UP: Beendet einen Indent-Block.
# indent_end(&stream);
# > stream: Stream
# < stream: Stream
# verändert STACK
  local void indent_end (const object* stream_);
  local void indent_end(stream_)
    var const object* stream_;
    {
      if (!(builtin_stream_p(*stream_)
            && (TheStream(*stream_)->strmtype == strmtype_pphelp)
         ) ) {
        # normaler Stream -> nichts zu tun
      } else {
        # Pretty-Print-Hilfs-Stream
        # die beiden Bindungen von INDENT_START auflösen:
        dynamic_unbind();
        dynamic_unbind();
      }
    }

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
    {
      var object stream = *stream_;
      if (!(builtin_stream_p(stream)
            && (TheStream(stream)->strmtype == strmtype_pphelp)
         ) ) {
        # normaler Stream -> nichts zu tun
      } else {
        # Pretty-Print-Hilfs-Stream
        # Line-Position merken:
        pushSTACK(TheStream(stream)->strm_pphelp_lpos);
      }
    }

# UP: Subtrahiert die Positionen, liefert die Einrückungsbreite.
# indentprep_end(&stream)
# > stream: Stream
# < stream: Stream
# < ergebnis: Einrückungsbreite
# verändert STACK
  local uintL indentprep_end (const object* stream_);
  local uintL indentprep_end(stream_)
    var const object* stream_;
    {
      var object stream = *stream_;
      if (!(builtin_stream_p(stream)
            && (TheStream(stream)->strmtype == strmtype_pphelp)
         ) ) {
        return 0; # normaler Stream -> nichts zu tun
      } else {
        # Pretty-Print-Hilfs-Stream
        var uintL lpos_now = # jetzige Line-Position
          posfixnum_to_L(TheStream(stream)->strm_pphelp_lpos);
        var uintL lpos_before = # gemerkte Line-Position
          posfixnum_to_L(popSTACK());
        return (lpos_now>=lpos_before ? lpos_now-lpos_before : 0);
      }
    }

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
# if yes: can trigger GC
# if no: changes STACK
  local bool level_check (const object* stream_);
  local bool level_check(stream_)
    var const object* stream_;
    {
      var object level = Symbol_value(S(prin_level)); # SYS::*PRIN-LEVEL*, ein Fixnum >=0
      var object limit = Symbol_value(S(print_level)); # *PRINT-LEVEL*
      if (!test_value(S(print_readably))
          && posfixnump(limit) # Beschränkung vorhanden?
          && (posfixnum_to_L(level) >= posfixnum_to_L(limit)) # und erreicht oder überschritten?
         ) {
        # ja -> '#' ausgeben und herausspringen:
        pr_level(stream_); return true;
      } else {
        # nein -> *PRINT-LEVEL* noch unerreicht.
        # binde SYS::*PRIN-LEVEL* an (1+ SYS::*PRIN-LEVEL*) :
        level = fixnum_inc(level,1); # (incf level)
        dynamic_bind(S(prin_level),level);
        return false;
      }
    }

# UP: Beendet einen Block mit erhöhtem SYS::*PRIN-LEVEL*.
# level_end(&stream);
# > stream: Stream
# < stream: Stream
# verändert STACK
  local void level_end (const object* stream_);
  local void level_end(stream_)
    var const object* stream_;
    {
      dynamic_unbind();
    }

# ------------------ Unterprogramme für *PRINT-LENGTH* ------------------------

# Length
# ------

# UP: Liefert die Längengrenze für strukturierte Objekte wie z.B. Listen.
# get_print_length()
# < ergebnis: Längengrenze
  local uintL get_print_length (void);
  local uintL get_print_length()
    {
      var object limit = Symbol_value(S(print_length)); # *PRINT-LENGTH*
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
#      sonst: ergebnis->flag: true, falls obj als #n=... auszugeben ist
#                             false, falls obj als #n# auszugeben ist
#             ergebnis->n: n
#             ergebnis->ptr: Im Fall #n=... ist vor der Ausgabe
#                            das Fixnum *ptr zu incrementieren.
  typedef struct {
    bool flag;
    uintL n;
    object* ptr;
  } circle_info;
  local circle_info* circle_p (object obj);
  local circle_info* circle_p(obj)
    var object obj;
    {
      # *PRINT-CIRCLE* abfragen:
      if (test_value(S(print_circle))) {
        var object table = Symbol_value(S(print_circle_table)); # SYS::*PRINT-CIRCLE-TABLE*
        if (!simple_vector_p(table)) { # sollte ein Simple-Vector sein !
         bad_table:
          dynamic_bind(S(print_circle),NIL); # *PRINT-CIRCLE* an NIL binden
          pushSTACK(S(print_circle_table)); # SYS::*PRINT-CIRCLE-TABLE*
          pushSTACK(S(print));
          fehler(error,
                 GETTEXT("~: the value of ~ has been arbitrarily altered")
                );
        }
        # Durch den Vektor table = #(i ...) mit m+1 (0<=i<=m) Elementen
        # durchlaufen:
        # Kommt obj unter den Elementen 1,...,i vor -> Fall false, n:=Index.
        # Kommt obj unter den Elementen i+1,...,m vor -> bringe
        #   obj an die Stelle i+1, Fall true, n:=i+1, nachher i:=i+1.
        # Sonst Fall NULL.
        var local circle_info info; # Platz für die Rückgabe der Werte
        var uintL m1 = Svector_length(table); # Länge m+1
        if (m1==0) goto bad_table; # sollte >0 sein!
        var object* ptr = &TheSvector(table)->data[0]; # Pointer in den Vektor
        var uintL i = posfixnum_to_L(*ptr++); # erstes Element i
        var uintL index = 1;
        until (index == m1) { # Schleife m mal durchlaufen
          if (eq(*ptr++,obj)) # obj mit nächstem Vektor-Element vergleichen
            goto found;
          index++;
        }
        # nicht gefunden -> fertig
        goto normal;
       found: # obj als Vektor-Element index gefunden, 1 <= index <= m,
              # ptr = &TheSvector(table)->data[index+1] .
        if (index <= i) {
          # obj ist als #n# auszugeben, n=index.
          info.flag = false; info.n = index; return &info;
        } else {
          # obj an Position i+1 bringen:
          i = i+1;
          # (rotatef (svref Vektor i) (svref Vektor index)) :
          {
            var object* ptr_i = &TheSvector(table)->data[i];
            *--ptr = *ptr_i; *ptr_i = obj;
          }
          # obj ist als #n=... auszugeben, n=i.
          info.flag = true; info.n = i;
          info.ptr = &TheSvector(table)->data[0]; # nachher i im Vektor erhöhen
          return &info;
        }
      }
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
    {
      # Feststellen, ob Zirkularität:
      var circle_info* info = circle_p(obj);
      if (info == (circle_info*)NULL) {
        # keine Zirkularität, obj normal ausgeben:
        (*pr_xxx)(stream_,obj);
      } else {
        # Zirkularität
        if (info->flag) {
          # obj als #n=... ausgeben:
          # erst noch für circle_p das Fixnum im Vektor incrementieren:
          {
            var object* ptr = info->ptr;
            *ptr = fixnum_inc(*ptr,1);
          }
          {
            var uintL n = info->n;
            pushSTACK(obj); # obj retten
            # Präfix ausgeben und Einrückungstiefe berechnen:
            INDENTPREP_START;
            write_ascii_char(stream_,'#');
            pr_uint(stream_,n);
            write_ascii_char(stream_,'=');
          }
          {
            var uintL indent = INDENTPREP_END;
            obj = popSTACK(); # obj zurück
            # obj (eingerückt) ausgeben:
            INDENT_START(indent);
            (*pr_xxx)(stream_,obj);
            INDENT_END;
          }
        } else {
          # obj als #n# ausgeben:
          var uintL n = info->n;
          write_ascii_char(stream_,'#');
          pr_uint(stream_,n);
          write_ascii_char(stream_,'#');
        }
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
    {
      # Streamtyp (PPHELP-Stream oder nicht) muss zu *PRINT-PRETTY* passen.
      if (test_value(S(print_pretty))) {
        # *PRINT-PRETTY* /= NIL.
        # Falls *stream_ kein PPHELP-Stream ist,
        # muss er durch einen PPHELP-Stream ersetzt werden:
        if (!(builtin_stream_p(*stream_)
              && (TheStream(*stream_)->strmtype == strmtype_pphelp)
           ) ) {
          # noch ein normaler Stream
          dynamic_bind(S(prin_l1),Fixnum_0); # SYS::*PRIN-L1* an 0 binden
          dynamic_bind(S(prin_lm),Fixnum_0); # SYS::*PRIN-LM* an 0 binden
          pushSTACK(obj); # Objekt retten
          # SYS::*PRIN-L1* auf dessen Line-Position setzen:
          {
            var object linepos = get_line_position(*stream_);
            if (!posfixnump(linepos))
              linepos = Fixnum_0;
            Symbol_value(S(prin_l1)) = linepos;
          }
          pushSTACK(make_pphelp_stream()); # neuer PPHELP-Stream, Line-Position = 0
          if (stream_get_read_eval(*stream_))
            TheStream(STACK_0)->strmflags |= bit(strmflags_reval_bit_B); # READ-EVAL-Bit übernehmen
          # Objekt auf den neuen Stream ausgeben:
          (*pr_xxx)(&STACK_0,STACK_1);
          # Inhalt des neuen Streams auf den alten Stream ausgeben:
          {
            var object ppstream = popSTACK(); # der neue Stream
            STACK_0 = nreverse(TheStream(ppstream)->strm_pphelp_strings); # Liste von Output-Zeilen
            # Falls es ein Mehrzeiler wurde, der nicht mit einem Newline
            # anfängt, und die alte Line-Position >0 ist,
            # zuerst noch ein Newline auf den alten Stream ausgeben:
            if (eq(TheStream(ppstream)->strm_pphelp_modus,einzeiler) # Einzeiler ?
                || nullp(Symbol_value(S(pprint_first_newline))))
              goto skip_first_NL; # in die Schleife
            {
              var object firststring = Car(STACK_0); # erste Zeile, ein Semi-Simple-String
              if ((TheIarray(firststring)->dims[1] == 0) # leer?
                  || chareq(TheSstring(TheIarray(firststring)->data)->data[0],ascii(NL)) # oder Newline am Anfang?
                 )
                goto skip_first_NL; # in die Schleife
            }
            if (eq(Symbol_value(S(prin_l1)),Fixnum_0)) # oder ab Position 0 ?
              goto skip_first_NL; # in die Schleife
          }
          do {
            write_ascii_char(stream_,NL); # #\Newline als Trennzeichen zwischen den Zeilen
           skip_first_NL:
            # nichtleere Stringliste STACK_0 auf den Stream ausgeben:
            var object list = STACK_0;
            STACK_0 = Cdr(list);
            write_string(stream_,Car(list)); # einzelnen String ausgeben
          } while (mconsp(STACK_0));
          skipSTACK(1);
          dynamic_unbind();
          dynamic_unbind();
        } else {
          # schon ein PPHELP-Stream
          (*pr_xxx)(stream_,obj);
        }
      } else {
        # *PRINT-PRETTY* = NIL.
        # Falls *stream_ ein PPHELP-Stream ist, muss er durch einen
        # einelementigen Broadcast-Stream ersetzt werden:
        if (!(builtin_stream_p(*stream_)
              && (TheStream(*stream_)->strmtype == strmtype_pphelp)
           ) ) {
          # normaler Stream
          (*pr_xxx)(stream_,obj);
        } else {
          # ein PPHELP-Stream
          pushSTACK(obj);
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
    {
      # Falls *PRINT-CIRCLE* /= NIL, in obj nach Zirkularitäten suchen.
      if (test_value(S(print_circle)) || test_value(S(print_readably))) {
        # Zirkularitäten suchen:
        pushSTACK(obj);
        var object circularities = # Zirkularitätentabelle
          get_circularities(obj,
                            test_value(S(print_array)) || test_value(S(print_readably)), # /= 0 genau dann wenn *PRINT-ARRAY* /= NIL
                            test_value(S(print_closure)) || test_value(S(print_readably)) # /= 0 genau dann wenn *PRINT-CLOSURE* /= NIL
                           );
        obj = popSTACK();
        if (nullp(circularities)) {
          # Keine Zirkularitäten festgestellt.
          # Kann *PRINT-CIRCLE* an NIL binden.
          dynamic_bind(S(print_circle),NIL);
          pr_enter_1(stream_,obj,pr_xxx);
          dynamic_unbind();
        } elif (eq(circularities,T)) {
          # Stacküberlauf trat auf
          # Überlauf der GET_CIRCULARITIES-Routine behandeln:
          dynamic_bind(S(print_circle),NIL); # *PRINT-CIRCLE* an NIL binden
          pushSTACK(S(print));
          fehler(storage_condition,
                 GETTEXT("~: not enough stack space for carrying out circularity analysis")
                );
        } else {
          # Zirkularitätenvektor
          # Binde SYS::*PRINT-CIRCLE-TABLE* an den Simple-Vector:
          dynamic_bind(S(print_circle_table),circularities);
          if (!test_value(S(print_circle))) {
            # *PRINT-READABLY* erzwingt *PRINT-CIRCLE* = T
            dynamic_bind(S(print_circle),T);
            pr_enter_1(stream_,obj,pr_xxx);
            dynamic_unbind();
          } else {
            pr_enter_1(stream_,obj,pr_xxx);
          }
          dynamic_unbind();
        }
      } else {
        pr_enter_1(stream_,obj,pr_xxx);
      }
    }
  # Dasselbe mit Behandlung von *PRINT-CIRCLE* und *PRINT-PRETTY*
  # und SYS::*PRIN-STREAM* :
  local void pr_enter (const object* stream_, object obj, pr_routine* pr_xxx);
  local void pr_enter(stream_,obj,pr_xxx)
    var const object* stream_;
    var object obj;
    var pr_routine* pr_xxx;
    {
      # Wert von SYS::*PRIN-STREAM* = dieser Stream ?
      if (eq(Symbol_value(S(prin_stream)),*stream_)) {
        # ja -> rekursiver Aufruf
        # Falls SYS::*PRINT-CIRCLE-TABLE* = #<UNBOUND> (was bedeutet, dass
        # *PRINT-CIRCLE* vorher NIL war) und jetzt *PRINT-CIRCLE* /= NIL
        # ist, muss Objekt obj nach Zirkularitäten abgesucht werden.
        if (eq(Symbol_value(S(print_circle_table)),unbound)) {
          pr_enter_2(stream_,obj,pr_xxx);
        } else {
          pr_enter_1(stream_,obj,pr_xxx);
        }
      } else {
        # nein -> nichtrekursiver Aufruf
        #if STACKCHECKP
        var object* STACKbefore = STACK; # STACK aufheben für später
        #endif
        dynamic_bind(S(prin_level),Fixnum_0); # SYS::*PRIN-LEVEL* an 0 binden
        dynamic_bind(S(prin_bqlevel),Fixnum_0); # SYS::*PRIN-BQLEVEL* an 0 binden
        dynamic_bind(S(prin_l1),Fixnum_0); # SYS::*PRIN-L1* an 0 binden (für Pretty-Print)
        dynamic_bind(S(prin_lm),Fixnum_0); # SYS::*PRIN-LM* an 0 binden (für Pretty-Print)
        dynamic_bind(S(prin_traillength),Fixnum_0); # bind SYS::*PRIN-TRAILLENGTH*
        pr_enter_2(stream_,obj,pr_xxx);
        dynamic_unbind();
        dynamic_unbind();
        dynamic_unbind();
        dynamic_unbind();
        dynamic_unbind();
        #if STACKCHECKP
        # Überprüfen, ob Stack aufgeräumt:
        if (!(STACK == STACKbefore))
          abort(); # wenn nicht, in den Debugger
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
    {
      var uintC count = 1;
      # *PRINT-CIRCLE* beachten:
      if (!test_value(S(print_circle))) {
        # *PRINT-CIRCLE* = NIL ->
        # Für den Fall, dass *PRINT-CIRCLE* an T gebunden werden wird,
        # muss SYS::*PRINT-CIRCLE-TABLE* an #<UNBOUND> gebunden werden
        # (es sei denn, es ist bereits = #<UNBOUND>).
        if (!eq(Symbol_value(S(print_circle_table)),unbound)) {
          dynamic_bind(S(print_circle_table),unbound); count++;
        }
      }
      # *PRINT-READABLY* beachten:
      if (test_value(S(print_readably))) {
        # Damit die benutzerdefinierten Print-Funktionen, die noch nichts
        # von *PRINT-READABLY* wissen, sich trotzdem danach benehmen,
        # binden wir die anderen Printer-Variablen passend:
        # *PRINT-READABLY* erzwingt *PRINT-ESCAPE* = T :
        if (!test_value(S(print_escape))) {
          dynamic_bind(S(print_escape),T); count++;
        }
        # *PRINT-READABLY* erzwingt *PRINT-BASE* = 10 :
        if (!eq(Symbol_value(S(print_base)),fixnum(10))) {
          dynamic_bind(S(print_base),fixnum(10)); count++;
        }
        # *PRINT-READABLY* erzwingt *PRINT-RADIX* = T :
        if (!test_value(S(print_radix))) {
          dynamic_bind(S(print_radix),T); count++;
        }
        # *PRINT-READABLY* erzwingt *PRINT-CIRCLE* = T :
        if (!test_value(S(print_circle))) {
          dynamic_bind(S(print_circle),T); count++;
        }
        # *PRINT-READABLY* erzwingt *PRINT-LEVEL* = NIL :
        if (test_value(S(print_level))) {
          dynamic_bind(S(print_level),NIL); count++;
        }
        # *PRINT-READABLY* erzwingt *PRINT-LENGTH* = NIL :
        if (test_value(S(print_length))) {
          dynamic_bind(S(print_length),NIL); count++;
        }
        # *PRINT-READABLY* erzwingt *PRINT-GENSYM* = T :
        if (!test_value(S(print_gensym))) {
          dynamic_bind(S(print_gensym),T); count++;
        }
        # *PRINT-READABLY* erzwingt *PRINT-ARRAY* = T :
        if (!test_value(S(print_array))) {
          dynamic_bind(S(print_array),T); count++;
        }
        # *PRINT-READABLY* erzwingt *PRINT-CLOSURE* = T :
        if (!test_value(S(print_closure))) {
          dynamic_bind(S(print_closure),T); count++;
        }
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
  local void pr_symbol_part (const object* stream_, object string, bool case_sensitive);
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
    {
     restart_it:
      # Auf Tastatur-Interrupt testen:
      interruptp({
        pushSTACK(obj); # obj retten, stream ist im STACK sicher
        pushSTACK(S(print)); tast_break(); # PRINT ruft Break-Schleife auf
        obj = popSTACK(); # obj zurück
        goto restart_it;
      });
      # auf Stacküberlauf testen:
      check_SP(); check_STACK();
      # Zirkularität behandeln:
      pr_circle(stream_,obj,&prin_object_dispatch);
    }
  local void prin_object_dispatch(stream_,obj)
    var const object* stream_;
    var object obj;
    {
      # Nach der Typinfo verzweigen:
      #ifdef TYPECODES
      switch (typecode(obj)) {
        case_machine: # Maschinenpointer
          pr_machine(stream_,obj); break;
        case_string: # String
          pr_string(stream_,obj); break;
        case_bvector: # Bit-Vektor
          pr_bvector(stream_,obj); break;
        case_b2vector:
        case_b4vector:
        case_b8vector:
        case_b16vector:
        case_b32vector:
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
          if (as_oint(obj) & wbit(0 + oint_addr_shift)) {
            if (as_oint(obj) & wbit(oint_data_len-1 + oint_addr_shift)) {
              # System-Pointer
              pr_system(stream_,obj);
            } else {
              # Read-Label
              pr_readlabel(stream_,obj);
            }
          } else {
            # Frame-Pointer
            pr_framepointer(stream_,obj);
          }
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
      if (orecordp(obj))
        pr_orecord(stream_,obj);
      elif (consp(obj))
        pr_cons(stream_,obj);
      elif (immediate_number_p(obj))
        pr_number(stream_,obj);
      elif (charp(obj))
        pr_character(stream_,obj);
      elif (subrp(obj))
        pr_subr(stream_,obj);
      elif (machinep(obj))
        pr_machine(stream_,obj);
      elif (read_label_p(obj))
        pr_readlabel(stream_,obj);
      elif (systemp(obj))
        pr_system(stream_,obj);
      else
        NOTREACHED
      #endif
    }


# ------------- PRINT-Routinen für verschiedene Datentypen --------------------

#                      -------- Symbole --------

# UP: print a symbol into a stream
# pr_symbol(&stream,sym);
# > sym: Symbol
# > stream: Stream
# < stream: Stream
# can trigger GC
  local void pr_symbol(stream_,sym)
    var const object* stream_;
    var object sym;
    {
      # *PRINT-ESCAPE* abfragen:
      if (test_value(S(print_escape)) || test_value(S(print_readably))) {
        # mit Escape-Zeichen und evtl. Packagenamen:
        var bool case_sensitive = false;
        var object curr_pack = get_current_package();
        if (accessiblep(sym,curr_pack) &&
            # print PACK::SYMBOL even when the symbol is accessble
            # this is for writing compiled files
            nullp(Symbol_value(S(print_symbols_long)))) {
          # Falls Symbol accessible und nicht verdeckt,
          # keinen Packagenamen und keine Packagemarker ausgeben.
          case_sensitive = pack_casesensitivep(curr_pack);
        } else {
          # Sonst:
          var object home;
          pushSTACK(sym); # Symbol retten
          if (keywordp(sym)) # Keyword ?
            goto one_marker; # ja -> nur 1 Packagemarker ausgeben
          home = Symbol_package(sym); # Home-package des Symbols
          if (nullp(home)) {
            # uninterniertes Symbol ausgeben
            # *PRINT-GENSYM* abfragen:
            if (test_value(S(print_gensym)) || test_value(S(print_readably))) {
              # Syntax #:name verwenden
              write_ascii_char(stream_,'#'); goto one_marker;
            }
            # sonst ohne Präfix ausgeben
          } else {
            # Symbol mit Packagenamen und 1 oder 2 Packagemarkern ausgeben
            pushSTACK(home); # Home-Package retten
            pr_symbol_part(stream_,ThePackage(home)->pack_name,false); # Packagenamen ausgeben
            home = popSTACK(); # Home-Package zurück
            case_sensitive = pack_casesensitivep(home);
            if (externalp(STACK_0,home) &&
                # the "raison d'etre" of *PRINT-SYMBOLS-LONG* is FAS files,
                # so it forces even external symbols to be printed with "::"
                nullp(Symbol_value(S(print_symbols_long))))
              goto one_marker; # ja -> 1 Packagemarker
            write_ascii_char(stream_,':'); # sonst 2 Packagemarker
           one_marker:
            write_ascii_char(stream_,':');
          }
          sym = popSTACK(); # sym zurück
        }
        pr_symbol_part(stream_,Symbol_name(sym),case_sensitive); # Symbolnamen ausgeben
      } else {
        # Symbol ohne Escape-Zeichen ausgeben:
        # nur den Symbolnamen unter Kontrolle von *PRINT-CASE* ausgeben
        write_sstring_case(stream_,Symbol_name(sym));
      }
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
    var bool case_sensitive;
    {
      # Feststellen, ob der Name ohne |...| außenrum ausgegeben werden kann:
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
      if (len==0)
        goto surround; # Länge=0 -> muss |...| verwenden
      # Bedingungen 2-4 überprüfen:
      {
        # Brauche die Attributcodetabelle und die aktuelle Syntaxcodetabelle:
        var object syntax_table; # Syntaxcodetabelle, char_code_limit Elemente
        var uintW rtcase; # readtable-case
        {
          var object readtable;
          get_readtable(readtable = ); # aktuelle Readtable
          syntax_table = TheReadtable(readtable)->readtable_syntax_table;
          rtcase = posfixnum_to_L(TheReadtable(readtable)->readtable_case);
        }
        # String durchlaufen:
        SstringDispatch(string,
          {
            var const chart* charptr = &TheSstring(string)->data[0];
            var uintL count = len;
            var chart c = *charptr++; # erstes Character
            # sein Syntaxcode soll Constituent sein:
            if (!(syntax_table_get(syntax_table,c) == syntax_constituent))
              goto surround; # nein -> muss |...| verwenden
            loop {
              if (attribute_of(c) == a_pack_m) # Attributcode Package-Marker ?
                goto surround; # ja -> muss |...| verwenden
              if (!case_sensitive)
                switch (rtcase) {
                  case case_upcase:
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
              c = *charptr++; # the next character
              switch (syntax_table_get(syntax_table,c)) { # sein Syntaxcode
                case syntax_constituent:
                case syntax_nt_macro:
                  break;
                default: # Syntaxcode /= Constituent, Nonterminating Macro
                  goto surround; # -> muss |...| verwenden
              }
            }
          },
          {
            var const scint* charptr = &TheSmallSstring(string)->data[0];
            var uintL count = len;
            var chart c = as_chart(*charptr++); # erstes Character
            # sein Syntaxcode soll Constituent sein:
            if (!(syntax_table_get(syntax_table,c) == syntax_constituent))
              goto surround; # nein -> muss |...| verwenden
            loop {
              if (attribute_of(c) == a_pack_m) # Attributcode Package-Marker ?
                goto surround; # ja -> muss |...| verwenden
              if (!case_sensitive)
                switch (rtcase) {
                  case case_upcase:
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
              c = as_chart(*charptr++); # the next character
              switch (syntax_table_get(syntax_table,c)) { # sein Syntaxcode
                case syntax_constituent:
                case syntax_nt_macro:
                  break;
                default: # Syntaxcode /= Constituent, Nonterminating Macro
                  goto surround; # -> muss |...| verwenden
              }
            }
          }
          );
      }
      # Bedingung 5 überprüfen:
      {
        pushSTACK(string); # String retten
        get_buffers(); # zwei Buffer allozieren, in den STACK
        # und füllen:
        SstringDispatch(STACK_2,
          {
            var uintL index = 0;
            until (index == len) {
              var chart c = TheSstring(STACK_2)->data[index]; # the next character
              ssstring_push_extend(STACK_1,c); # in den Character-Buffer
              ssbvector_push_extend(STACK_0,attribute_of(c)); # und in den Attributcode-Buffer
              index++;
            }
          },
          {
            var uintL index = 0;
            until (index == len) {
              var chart c = as_chart(TheSmallSstring(STACK_2)->data[index]); # the next character
              ssstring_push_extend(STACK_1,c); # in den Character-Buffer
              ssbvector_push_extend(STACK_0,attribute_of(c)); # und in den Attributcode-Buffer
              index++;
            }
          }
          );
        O(token_buff_2) = popSTACK(); # Attributcode-Buffer
        O(token_buff_1) = popSTACK(); # Character-Buffer
        string = popSTACK(); # String zurück
        if (test_dots()) # nur Punkte -> muss |...| verwenden
          goto surround;
        # Potential-Number-Syntax?
        {
          var uintWL base = get_print_base(); # Wert von *PRINT-BASE*
          var token_info info;
          if (test_potential_number_syntax(&base,&info))
            goto surround; # ja -> muss |...| verwenden
        }
      }
      # Name kann ohne Escape-Characters ausgegeben werden.
      if (case_sensitive)
        write_sstring(stream_,string);
      else
        # Dabei jedoch *PRINT-CASE* beachten:
        write_sstring_case(stream_,string);
      return;
     surround: # Namen unter Verwendung der Escape-Character |...| ausgeben:
      # Syntaxcodetabelle holen:
      {
        var object readtable;
        get_readtable(readtable = ); # aktuelle Readtable
        pushSTACK(TheReadtable(readtable)->readtable_syntax_table);
      }
      pushSTACK(string);
      # stack layout: syntax_table, string.
      write_ascii_char(stream_,'|');
      SstringDispatch(STACK_0,
        {
          var uintL index = 0;
          until (index == len) {
            var chart c = TheSstring(STACK_0)->data[index]; # the next character
            switch (syntax_table_get(STACK_1,c)) { # dessen Syntaxcode
              case syntax_single_esc:
              case syntax_multi_esc:
                # Dem Escape-Character c wird ein '\' vorangestellt:
                write_ascii_char(stream_,'\\');
              default: ;
            }
            write_code_char(stream_,c); # Character ausgeben
            index++;
          }
        },
        {
          var uintL index = 0;
          until (index == len) {
            var chart c = as_chart(TheSmallSstring(STACK_0)->data[index]); # the next character
            switch (syntax_table_get(STACK_1,c)) { # dessen Syntaxcode
              case syntax_single_esc:
              case syntax_multi_esc:
                # Dem Escape-Character c wird ein '\' vorangestellt:
                write_ascii_char(stream_,'\\');
              default: ;
            }
            write_code_char(stream_,c); # Character ausgeben
            index++;
          }
        }
        );
      write_ascii_char(stream_,'|');
      skipSTACK(2);
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
    {
      # *PRINT-ESCAPE* abfragen:
      if (test_value(S(print_escape)) || test_value(S(print_readably))) {
        pr_symbol_part(stream_,string,pack_casesensitivep(get_current_package())); # mit Escape-Zeichen ausgeben
      } else {
        write_sstring_case(stream_,string); # ohne Escape-Zeichen ausgeben
      }
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
    {
      # *PRINT-ESCAPE* abfragen:
      if (test_value(S(print_escape)) || test_value(S(print_readably))) {
        # Character mit Escape-Zeichen ausgeben.
        # Syntax:  # \ char
        # bzw.     # \ charname
        write_ascii_char(stream_,'#');
        write_ascii_char(stream_,'\\');
        var chart code = char_code(ch); # Code
        if (as_cint(code) > 0x20 && as_cint(code) < 0x7F) {
          # graphic standard character -> don't even lookup the name
          write_code_char(stream_,code);
        } else {
          var object charname = char_name(code); # Name des Characters
          if (nullp(charname)) {
            # kein Name vorhanden
            write_code_char(stream_,code);
          } else {
            # Namen (Simple-String) ausgeben
            write_sstring_case(stream_,charname);
          }
        }
      } else {
        # Character ohne Escape-Zeichen ausgeben
        write_char(stream_,ch); # ch selbst ausgeben
      }
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
    {
      # *PRINT-ESCAPE* abfragen:
      if (test_value(S(print_escape)) || test_value(S(print_readably))) {
        # mit Escape-Zeichen:
        var uintL index = start;
        pushSTACK(string); # Simple-String retten
        write_ascii_char(stream_,'"'); # vorher ein Anführungszeichen
        string = STACK_0;
        #if 0
        SstringDispatch(string,
          {
            dotimesL(len,len, {
              var chart c = TheSstring(STACK_0)->data[index]; # nächstes Zeichen
              # bei c = #\" oder c = #\\ erst noch ein '\' ausgeben:
              if (chareq(c,ascii('"')) || chareq(c,ascii('\\')))
                write_ascii_char(stream_,'\\');
              write_code_char(stream_,c);
              index++;
            });
          },
          {
            dotimesL(len,len, {
              var chart c = as_chart(TheSmallSstring(STACK_0)->data[index]); # nächstes Zeichen
              # bei c = #\" oder c = #\\ erst noch ein '\' ausgeben:
              if (chareq(c,ascii('"')) || chareq(c,ascii('\\')))
                write_ascii_char(stream_,'\\');
              write_code_char(stream_,c);
              index++;
            });
          }
          );
        #else # dasselbe, etwas optimiert
        SstringDispatch(string,
          {
            var uintL index0 = index;
            loop {
              # Suche den nächsten #\" oder #\\ :
              string = STACK_0;
              while (len > 0) {
                var chart c = TheSstring(string)->data[index];
                if (chareq(c,ascii('"')) || chareq(c,ascii('\\')))
                  break;
                index++; len--;
              }
              if (!(index==index0))
                write_sstring_ab(stream_,string,index0,index-index0);
              if (len==0)
                break;
              write_ascii_char(stream_,'\\');
              index0 = index; index++; len--;
            }
          },
          {
            var uintL index0 = index;
            loop {
              # Suche den nächsten #\" oder #\\ :
              string = STACK_0;
              while (len > 0) {
                var chart c = as_chart(TheSmallSstring(string)->data[index]);
                if (chareq(c,ascii('"')) || chareq(c,ascii('\\')))
                  break;
                index++; len--;
              }
              if (!(index==index0))
                write_sstring_ab(stream_,string,index0,index-index0);
              if (len==0)
                break;
              write_ascii_char(stream_,'\\');
              index0 = index; index++; len--;
            }
          }
          );
        #endif
        write_ascii_char(stream_,'"'); # nachher ein Anführungszeichen
        skipSTACK(1);
      } else {
        # ohne Escape-Zeichen: nur write_sstring_ab
        write_sstring_ab(stream_,string,start,len);
      }
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
    {
      var uintL len = vector_length(string); # Länge
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
    {
      # Spezielle Listen sind die der Form
      # (QUOTE a), (FUNCTION a), (SYS::BACKQUOTE a [b]) und
      # (SYS::SPLICE a), (SYS::NSPLICE a), (SYS::UNQUOTE a)
      # falls SYS::*PRIN-BQLEVEL* > 0
      var object head = Car(obj);
      var pr_routine* pr_xxx;
      if (eq(head,S(quote))) { # QUOTE
        pr_xxx = &pr_list_quote; goto test2;
      } elif (eq(head,S(function))) { # FUNCTION
        pr_xxx = &pr_list_function; goto test2;
      } elif (eq(head,S(backquote))) { # SYS::BACKQUOTE
        pr_xxx = &pr_list_backquote;
        # Teste noch, ob obj eine Liste der Länge 2 oder 3 ist.
        obj = Cdr(obj); # Der CDR
        if (consp(obj) && # muss ein Cons sein,
            (obj = Cdr(obj), # der CDDR
             (atomp(obj) ? nullp(obj) : nullp(Cdr(obj))) # NIL oder eine einelementige Liste
           ))
          return pr_xxx;
        else
          return (pr_routine*)NULL;
      } elif (eq(head,S(splice))) { # SYS::SPLICE
        pr_xxx = &pr_list_splice; goto test2bq;
      } elif (eq(head,S(nsplice))) { # SYS::NSPLICE
        pr_xxx = &pr_list_nsplice; goto test2bq;
      } elif (eq(head,S(unquote))) { # SYS::UNQUOTE
        pr_xxx = &pr_list_unquote; goto test2bq;
      } else
        return (pr_routine*)NULL;
     test2bq: # Teste noch, ob SYS::*PRIN-BQLEVEL* > 0 und
              # obj eine Liste der Länge 2 ist.
      {
        var object bqlevel = Symbol_value(S(prin_bqlevel));
        if (!(posfixnump(bqlevel) && !eq(bqlevel,Fixnum_0)))
          return (pr_routine*)NULL;
      }
     test2: # Teste noch, ob obj eine Liste der Länge 2 ist.
      if (mconsp(Cdr(obj)) && nullp(Cdr(Cdr(obj))))
        return pr_xxx;
      else
        return (pr_routine*)NULL;
    }

# UP: Liefert den Wert des Fixnums *PRINT-INDENT-LISTS*.
# get_indent_lists()
# < ergebnis: Fixnum > 0
  local uintL get_indent_lists (void);
  local uintL get_indent_lists()
    {
      var object obj = Symbol_value(S(print_indent_lists));
      if (posfixnump(obj)) {
        var uintL indent = posfixnum_to_L(obj);
        if (indent > 0)
          return indent;
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
    {
      if (nullp(list)) {
        # NIL als () ausgeben:
        write_ascii_char(stream_,'('); write_ascii_char(stream_,')');
      } else {
        # ein Cons
        pr_cons(stream_,list);
      }
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
    {
      # Spezialfälle abfangen:
      {
        var pr_routine* special = special_list_p(list);
        if (!(special == (pr_routine*)NULL)) {
          (*special)(stream_,list); # spezielle pr_list_xxx-Routine aufrufen
          return;
        }
      }
      LEVEL_CHECK;
      {
        var uintL length_limit = get_print_length(); # *PRINT-LENGTH*-Begrenzung
        var uintL length = 0; # bisherige Länge := 0
        pushSTACK(list); # Liste retten
        var object* list_ = &STACK_0; # und merken, wo sie sitzt
        KLAMMER_AUF; # '('
        INDENT_START(get_indent_lists()); # um 1 Zeichen einrücken, wegen '('
        JUSTIFY_START(1);
        # auf Erreichen von *PRINT-LENGTH* prüfen:
        if (length_limit==0) goto dots;
        loop {
          # ab hier den CAR ausgeben
          list = *list_; *list_ = Cdr(list); # Liste verkürzen
          JUSTIFY_LAST(nullp(*list_));
          prin_object(stream_,Car(list)); # den CAR ausgeben
          length++; # Länge incrementieren
          # ab hier den Listenrest ausgeben
          if (nullp(*list_)) # Listenrest=NIL -> Listenende
            goto end_of_list;
          JUSTIFY_SPACE; # ein Space ausgeben
          if (matomp(*list_)) # Dotted List ?
            goto dotted_list;
          # auf Erreichen von *PRINT-LENGTH* prüfen:
          if (length >= length_limit)
            goto dots;
          # Prüfen, ob Dotted-List-Schreibweise nötig:
          list = *list_;
          if (!(circle_p(list) == (circle_info*)NULL)) # wegen Zirkularität nötig?
            goto dotted_list;
          if (!(special_list_p(list) == (pr_routine*)NULL)) # wegen QUOTE o.ä. nötig?
            goto dotted_list;
        }
       dotted_list: # Listenrest in Dotted-List-Schreibweise ausgeben:
        JUSTIFY_LAST(false);
        write_ascii_char(stream_,'.');
        JUSTIFY_SPACE;
        JUSTIFY_LAST(true);
        prin_object(stream_,*list_);
        goto end_of_list;
       dots: # Listenrest durch '...' abkürzen:
        JUSTIFY_LAST(true);
        write_ascii_char(stream_,'.');
        write_ascii_char(stream_,'.');
        write_ascii_char(stream_,'.');
        goto end_of_list;
       end_of_list: # Listeninhalt ausgegeben.
        JUSTIFY_END_ENG;
        INDENT_END;
        KLAMMER_ZU;
        skipSTACK(1);
      }
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
    {
      pushSTACK(Car(Cdr(list))); # (second list) retten
      write_ascii_char(stream_,'\''); # "'" ausgeben
      list = popSTACK();
      INDENT_START(1); # um 1 Zeichen einrücken wegen "'"
      prin_object(stream_,list); # object ausgeben
      INDENT_END;
    }

  local void pr_list_function(stream_,list) # list = (FUNCTION object)
    var const object* stream_;
    var object list;
    {
      pushSTACK(Car(Cdr(list))); # (second list) retten
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
    {
      pushSTACK(Car(Cdr(list))); # (second list) retten
      write_ascii_char(stream_,'`'); # '`' ausgeben
      list = popSTACK();
      # SYS::*PRIN-BQLEVEL* um 1 erhöhen:
      {
        var object bqlevel = Symbol_value(S(prin_bqlevel));
        if (!posfixnump(bqlevel))
          bqlevel = Fixnum_0;
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
    {
      pushSTACK(Car(Cdr(list))); # (second list) retten
      write_ascii_char(stream_,','); # Komma ausgeben
      write_char(stream_,ch); # '@' bzw. '.' ausgeben
      list = popSTACK();
      # SYS::*PRIN-BQLEVEL* um 1 verringern:
      dynamic_bind(S(prin_bqlevel),fixnum_inc(Symbol_value(S(prin_bqlevel)),-1));
      # Ist dies von der Form (UNQUOTE form) ?
      if (consp(list) && eq(Car(list),S(unquote))
          && mconsp(Cdr(list)) && nullp(Cdr(Cdr(list)))
         ) {
        # ja -> noch die Form ausgeben:
        list = Car(Cdr(list)); # (second object)
        INDENT_START(2); # um 2 Zeichen einrücken wegen ",@" bzw. ",."
        prin_object(stream_,list); # Form ausgeben
        INDENT_END;
      } else {
        # nein -> noch ein Quote und object ausgeben:
        pushSTACK(list); # object retten
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
    {
      pr_list_bothsplice(stream_,list,ascii_char('@'));
    }

  local void pr_list_nsplice(stream_,list) # list = (NSPLICE object)
    var const object* stream_;
    var object list;
    {
      pr_list_bothsplice(stream_,list,ascii_char('.'));
    }

  local void pr_list_unquote(stream_,list) # list = (UNQUOTE object)
    var const object* stream_;
    var object list;
    {
      pushSTACK(Car(Cdr(list))); # (second list) retten
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
    {
      if (R_rationalp(number)) {
        # rationale Zahl
        var uintWL base = get_print_base(); # Wert von *PRINT-BASE*
        # *PRINT-RADIX* abfragen:
        if (test_value(S(print_radix)) || test_value(S(print_readably))) {
          # Radix-Specifier ausgeben:
          pushSTACK(number); # number retten
          switch (base) {
            case 2: # Basis 2
              write_ascii_char(stream_,'#'); write_ascii_char(stream_,'b'); break;
            case 8: # Basis 8
              write_ascii_char(stream_,'#'); write_ascii_char(stream_,'o'); break;
            case 16: # Basis 16
              write_ascii_char(stream_,'#'); write_ascii_char(stream_,'x'); break;
            case 10: # Basis 10
              if (RA_integerp(number)) {
                # Basis 10 bei Integers durch nachgestellten Punkt
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
        if (RA_integerp(number)) {
          # Integer in Basis base ausgeben:
          print_integer(number,base,stream_);
        } else {
          # Ratio in Basis base ausgeben:
          pushSTACK(TheRatio(number)->rt_den); # Nenner retten
          print_integer(TheRatio(number)->rt_num,base,stream_); # Zähler ausgeben
          write_ascii_char(stream_,'/'); # Bruchstrich
          print_integer(popSTACK(),base,stream_); # Nenner ausgeben
        }
      } else {
        # Float
        print_float(number,stream_);
      }
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
    {
      if (N_realp(number)) {
        # reelle Zahl
        pr_real_number(stream_,number);
      } else {
        # komplexe Zahl
        pushSTACK(number); # Zahl retten
        var object* number_ = &STACK_0; # und merken, wo sie sitzt
        write_ascii_char(stream_,'#'); write_ascii_char(stream_,'C');
        KLAMMER_AUF;
        INDENT_START(3); # um 3 Zeichen einrücken, wegen '#C('
        JUSTIFY_START(1);
        JUSTIFY_LAST(false);
        pr_real_number(stream_,TheComplex(*number_)->c_real); # Realteil ausgeben
        JUSTIFY_SPACE;
        JUSTIFY_LAST(true);
        pr_real_number(stream_,TheComplex(*number_)->c_imag); # Imaginärteil ausgeben
        JUSTIFY_END_ENG;
        INDENT_END;
        KLAMMER_ZU;
        skipSTACK(1);
      }
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
    {
      pushSTACK(obj); # Array retten
      var object* obj_ = &STACK_0; # und merken, wo er sitzt
      write_ascii_char(stream_,'#'); write_ascii_char(stream_,'<');
      INDENT_START(2); # um 2 Zeichen einrücken, wegen '#<'
      JUSTIFY_START(1);
      JUSTIFY_LAST(false);
      write_sstring_case(stream_,O(printstring_array)); # "ARRAY" ausgeben
      JUSTIFY_SPACE;
      JUSTIFY_LAST(false);
      prin_object_dispatch(stream_,array_element_type(*obj_)); # Elementtyp (Symbol oder Liste) ausgeben
      JUSTIFY_SPACE;
      JUSTIFY_LAST(!array_has_fill_pointer_p(*obj_));
      pr_list(stream_,array_dimensions(*obj_)); # Dimensionsliste ausgeben
      if (array_has_fill_pointer_p(*obj_)) {
        # Array mit Fill-Pointer -> auch den Fill-Pointer ausgeben:
        JUSTIFY_SPACE;
        JUSTIFY_LAST(true);
        write_sstring_case(stream_,O(printstring_fill_pointer)); # "FILL-POINTER=" ausgeben
        pr_uint(stream_,vector_length(*obj_)); # Länge (=Fill-Pointer) ausgeben
      }
      JUSTIFY_END_ENG;
      INDENT_END;
      write_ascii_char(stream_,'>');
      skipSTACK(1);
    }

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
    {
      var uintL index = start;
      pushSTACK(bv); # Simple-Bit-Vektor retten
      write_ascii_char(stream_,'#'); write_ascii_char(stream_,'*');
      dotimesL(len,len, {
        write_char(stream_,
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
    {
      # *PRINT-ARRAY* abfragen:
      if (test_value(S(print_array)) || test_value(S(print_readably))) {
        # bv elementweise ausgeben:
        var uintL len = vector_length(bv); # Länge
        var uintL offset = 0; # Offset vom Bit-Vektor in den Datenvektor
        var object sbv = array_displace_check(bv,len,&offset); # Datenvektor
        pr_sbvector_ab(stream_,sbv,offset,len);
      } else {
        # *PRINT-ARRAY* = NIL -> in Kurzform ausgeben:
        pr_array_nil(stream_,bv);
      }
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
    {
      # *PRINT-ARRAY* abfragen:
      if (test_value(S(print_array)) || test_value(S(print_readably))) {
        # v elementweise ausgeben:
        LEVEL_CHECK;
        {
          var bool readable = # Flag, ob Länge und Typ mit ausgegeben werden
            (test_value(S(print_readably)) && !general_vector_p(v) ? true : false);
          var uintL length_limit = get_print_length(); # *PRINT-LENGTH*-Begrenzung
          var uintL length = 0; # bisherige Länge := 0
          # Vektor elementweise abarbeiten:
          var uintL len = vector_length(v); # Vektor-Länge
          var uintL offset = 0; # Offset vom Vektor in den Datenvektor
          {
            var object sv = array_displace_check(v,len,&offset); # Datenvektor
            pushSTACK(sv); # Simple-Vektor retten
          }
          var object* sv_ = &STACK_0; # und merken, wo er sitzt
          var uintL index = 0 + offset; # Startindex = 0 im Vektor
          if (readable) {
            write_ascii_char(stream_,'#'); write_ascii_char(stream_,'A');
            KLAMMER_AUF; # '(' ausgeben
            INDENT_START(3); # um 3 Zeichen einrücken, wegen '#A('
            JUSTIFY_START(1);
            JUSTIFY_LAST(false);
            prin_object_dispatch(stream_,array_element_type(*sv_)); # Elementtyp ausgeben
            JUSTIFY_SPACE;
            JUSTIFY_LAST(false);
            pushSTACK(fixnum(len));
            pr_list(stream_,listof(1)); # Liste mit der Länge ausgeben
            JUSTIFY_SPACE;
            JUSTIFY_LAST(true);
            KLAMMER_AUF; # '('
            INDENT_START(1); # um 1 Zeichen einrücken, wegen '('
          } else {
            write_ascii_char(stream_,'#');
            KLAMMER_AUF; # '('
            INDENT_START(2); # um 2 Zeichen einrücken, wegen '#('
          }
          JUSTIFY_START(1);
          for (; len > 0; len--) {
            # (außer vorm ersten Element) Space ausgeben:
            if (!(length==0))
              JUSTIFY_SPACE;
            # auf Erreichen von *PRINT-LENGTH* prüfen:
            if (length >= length_limit) {
              JUSTIFY_LAST(true);
              # Rest durch '...' abkürzen:
              write_ascii_char(stream_,'.');
              write_ascii_char(stream_,'.');
              write_ascii_char(stream_,'.');
              break;
            }
            JUSTIFY_LAST(len==1);
            # Vektorelement ausgeben:
            prin_object(stream_,storagevector_aref(*sv_,index));
            length++; # Länge incrementieren
            index++; # dann zum nächsten Vektor-Element
          }
          JUSTIFY_END_ENG;
          INDENT_END;
          KLAMMER_ZU;
          if (readable) {
            JUSTIFY_END_ENG;
            INDENT_END;
            KLAMMER_ZU;
          }
          skipSTACK(1);
        }
        LEVEL_END;
      } else {
        # *PRINT-ARRAY* = NIL -> in Kurzform ausgeben:
        pr_array_nil(stream_,v);
      }
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
  typedef struct {
    uintL index;
    uintL count;
  } pr_array_info;
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
    {
      # Element von allgemeinem Typ holen und ausgeben:
      prin_object(stream_,storagevector_aref(obj,info->index));
      info->index++;
    }

  local void pr_array_elt_bvector(stream_,obj,info)
    var const object* stream_;
    var object obj; # Simple-Bit-Vektor
    var pr_array_info* info;
    {
      # Teil-Bit-Vektor ausgeben:
      pr_sbvector_ab(stream_,obj,info->index,info->count);
      info->index += info->count;
    }

  local void pr_array_elt_string(stream_,obj,info)
    var const object* stream_;
    var object obj; # Simple-String
    var pr_array_info* info;
    {
      # Teil-String ausgeben:
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
  typedef struct {
    const object* stream_;
    const object* obj_;
    const array_dim_size* dims_sizes;
    pr_array_elt_routine* pr_one_elt;
    pr_array_info info;
    uintL length_limit;
  } pr_array_locals;
  local void pr_array_rekursion (pr_array_locals* locals, uintL depth);
  local void pr_array_rekursion(locals,depth)
    var pr_array_locals* locals;
    var uintL depth;
    {
      check_SP(); check_STACK();
      if (depth==0) {
        # Rekursionstiefe 0 -> Rekursionsbasis
        (*(locals->pr_one_elt)) # Funktion pr_one_elt aufrufen, mit
          (locals->stream_, # Streamadresse,
           *(locals->obj_), # Datenvektor obj,
           &(locals->info) # Infopointer
          ); # als Argumenten
        # Diese Funktion erhöht locals->info.index selbst.
      } else {
        depth--; # Rekursionstiefe verkleinern (noch >=0)
        var const object* stream_ = locals->stream_;
        var uintL length = 0; # bisherige Länge := 0
        var uintL endindex = locals->info.index # Start-Index im Datenvektor
                             + locals->dims_sizes[depth].dimprod # + Dimensionenprodukt
                             ; # liefert den End-Index dieses Teil-Arrays
        var uintL count = locals->dims_sizes[depth].dim;
        KLAMMER_AUF; # '(' ausgeben
        INDENT_START(1); # um 1 Zeichen einrücken, wegen '('
        JUSTIFY_START(1);
        # Schleife über Dimension (r-depth): jeweils einen Teil-Array ausgeben
        for (; count > 0; count--) {
          # (außer vorm ersten Teil-Array) Space ausgeben:
          if (!(length==0))
            JUSTIFY_SPACE;
          # auf Erreichen von *PRINT-LENGTH* prüfen:
          if (length >= locals->length_limit) {
            JUSTIFY_LAST(true);
            # Rest durch '...' abkürzen:
            write_ascii_char(stream_,'.');
            write_ascii_char(stream_,'.');
            write_ascii_char(stream_,'.');
            break;
          }
          JUSTIFY_LAST(count==1);
          # Teil-Array ausgeben:
          # (rekursiv, mit verkleinerter depth, und locals->info.index
          # wird ohne weiteres Zutun von einem Aufruf zum nächsten
          # weitergereicht)
          pr_array_rekursion(locals,depth);
          length++; # Länge incrementieren
          # locals->info.index ist schon incrementiert
        }
        JUSTIFY_END_WEIT;
        INDENT_END;
        KLAMMER_ZU; # ')' ausgeben
        locals->info.index = endindex; # jetzt am End-Index angelangt
      }
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
    {
      # *PRINT-ARRAY* abfragen:
      if (test_value(S(print_array)) || test_value(S(print_readably))) {
        # obj elementweise ausgeben:
        LEVEL_CHECK;
        {
          # Rang bestimmen und Dimensionen und Teilprodukte holen:
          var uintL r = (uintL)Iarray_rank(obj); # Rang
          var DYNAMIC_ARRAY(dims_sizes,array_dim_size,r); # dynamisch allozierter Array
          iarray_dims_sizes(obj,dims_sizes); # füllen
          var uintL depth = r; # Tiefe der Rekursion
          var pr_array_locals locals; # lokale Variablen
          var bool readable = true; # Flag, ob Dimensionen und Typ mit ausgegeben werden
          locals.stream_ = stream_;
          locals.dims_sizes = dims_sizes;
          locals.length_limit = get_print_length(); # Längenbegrenzung
          # Entscheidung über zu verwendende Routine:
          {
            var uintB atype = Iarray_flags(obj) & arrayflags_atype_mask;
            if ((r>0) && (locals.length_limit >= dims_sizes[0].dim)) {
              switch (atype) {
                case Atype_Bit:
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
                  readable = false; # automatisch wiedereinlesbar
                  goto routine_ok;
                default: ;
              }
            }
            locals.pr_one_elt = &pr_array_elt_t;
            locals.info.count = 1; # 1 als "Elementarlänge"
            if (atype==Atype_T)
              readable = false; # automatisch wiedereinlesbar
           routine_ok:
            locals.info.index = 0; # Start-Index ist 0
          }
          if (!test_value(S(print_readably)))
            readable = false; # braucht nicht wiedereinlesbar zu sein
          pushSTACK(obj); # Array retten
          var object* obj_ = &STACK_0; # und merken, wo er sitzt
          # Datenvektor holen:
          var uintL size = TheIarray(obj)->totalsize;
          if (size == 0)
            readable = true; # sonst weiß man nicht einmal die Dimensionen
          obj = iarray_displace_check(obj,size,&locals.info.index); # Datenvektor
          # locals.info.index = Offset vom Array in den Datenvektor
          pushSTACK(obj); locals.obj_ = &STACK_0; # obj im Stack unterbringen
          # Los geht's.
          if (readable) {
            write_ascii_char(stream_,'#'); write_ascii_char(stream_,'A');
            KLAMMER_AUF; # '(' ausgeben
            INDENT_START(3); # um 3 Zeichen einrücken, wegen '#A('
            JUSTIFY_START(1);
            JUSTIFY_LAST(false);
            prin_object_dispatch(stream_,array_element_type(*obj_)); # Elementtyp (Symbol oder Liste) ausgeben
            JUSTIFY_SPACE;
            JUSTIFY_LAST(false);
            pr_list(stream_,array_dimensions(*obj_)); # Dimensionsliste ausgeben
            JUSTIFY_SPACE;
            JUSTIFY_LAST(true);
            pr_array_rekursion(&locals,depth); # Array-Elemente ausgeben
            JUSTIFY_END_ENG;
            INDENT_END;
            KLAMMER_ZU; # ')' ausgeben
          } else {
            # Erst Präfix #nA ausgeben:
            INDENTPREP_START;
            write_ascii_char(stream_,'#');
            pr_uint(stream_,r); # Rang dezimal ausgeben
            write_ascii_char(stream_,'A');
            {
              var uintL indent = INDENTPREP_END;
            # Dann die Array-Elemente ausgeben:
              INDENT_START(indent);
            }
            pr_array_rekursion(&locals,depth);
            INDENT_END;
          }
          skipSTACK(2);
          FREE_DYNAMIC_ARRAY(dims_sizes);
        }
        LEVEL_END;
      } else {
        # *PRINT-ARRAY* = NIL -> in Kurzform ausgeben:
        pr_array_nil(stream_,obj);
      }
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
    {
      var object stream = *stream_;
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
    {
      var object stream = *stream_;
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
    {
      LEVEL_CHECK;
      # Typ der Structure bestimmen (vgl. TYPE-OF):
      {
        var object name = Car(TheStructure(structure)->structure_types);
        # name = (car '(name_1 ... name_i-1 name_i)) = name_1.
        # (GET name 'SYS::STRUCTURE-PRINT) ausführen:
        var object fun = get(name,S(structure_print));
        if (!eq(fun,unbound)) {
          # vorgegebene Print-Funktion aufrufen:
          pr_structure_external(stream_,structure,fun);
        } else {
          # keine vorgegebene Print-Funktion gefunden.
          # CLOS:PRINT-OBJECT aufrufen:
          pr_instance(stream_,structure);
        }
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
  local bool some_printable_slots (object slotlist);
  local bool some_printable_slots(slotlist)
    var object slotlist;
    {
      while (consp(slotlist)) {
        var object slot = Car(slotlist);
        if (simple_vector_p(slot) && (Svector_length(slot) >= 7)
            && !nullp(TheSvector(slot)->data[0]))
          return true;
        slotlist = Cdr(slotlist);
      }
      return false;
    }
  local void pr_structure_default(stream_,structure)
    var const object* stream_;
    var object structure;
    {
      var object name = Car(TheStructure(structure)->structure_types);
      # name = (car '(name_1 ... name_i-1 name_i)) = name_1.
      pushSTACK(structure);
      pushSTACK(name);
      var object* structure_ = &STACK_1;
      # Es ist *(structure_ STACKop 0) = structure
      # und    *(structure_ STACKop -1) = name .
      # (GET name 'SYS::DEFSTRUCT-DESCRIPTION) ausführen:
      var object description = get(name,S(defstruct_description));
      if (!eq(description,unbound)) {
        # Structure mit Slot-Namen ausgeben:
        pushSTACK(description);
        # stack layout: structure, name, description.
        # description muss ein Simple-Vector der Länge >=4 sein !
        if (!(simple_vector_p(description)
              && (Svector_length(description) >= 4)
           ) ) {
         bad_description:
          pushSTACK(S(defstruct_description));
          pushSTACK(S(print));
          fehler(error,
                 GETTEXT("~: bad ~")
                );
        }
        var bool readable = # true falls (svref description 2) /= NIL
          !nullp(TheSvector(description)->data[2]);
        if (readable) {
          # Structure wiedereinlesbar ausgeben:
          write_ascii_char(stream_,'#'); write_ascii_char(stream_,'S');
          KLAMMER_AUF;
          INDENT_START(3); # um 3 Zeichen einrücken, wegen '#S('
        } else {
          # Structure nicht wiedereinlesbar ausgeben:
          if (test_value(S(print_readably)))
            fehler_print_readably(*structure_);
          write_ascii_char(stream_,'#'); write_ascii_char(stream_,'<');
          INDENT_START(2); # um 2 Zeichen einrücken, wegen '#<'
        }
        JUSTIFY_START(1);
        pushSTACK(TheSvector(*(structure_ STACKop -2))->data[3]);
        JUSTIFY_LAST(!some_printable_slots(STACK_0));
        prin_object(stream_,*(structure_ STACKop -1)); # name ausgeben
        # Slot-Liste STACK_0 = (svref description 3) durchlaufen:
        {
          var uintL length_limit = get_print_length(); # *PRINT-LENGTH*-Begrenzung
          var uintL length = 0; # bisherige Länge := 0
          while (mconsp(STACK_0)) {
            var object slot = STACK_0;
            STACK_0 = Cdr(slot); # Liste verkürzen
            slot = Car(slot); # ein einzelner slot
            if (!(simple_vector_p(slot)
                  && (Svector_length(slot) >= 7)
               ) )
              goto bad_description; # sollte ein ds-slot sein
            if (!nullp(TheSvector(slot)->data[0])) { # Slot #(NIL ...) übergehen
              pushSTACK(slot); # save slot
              JUSTIFY_SPACE; # Space ausgeben
              # auf Erreichen von *PRINT-LENGTH* prüfen:
              if (length >= length_limit) {
                JUSTIFY_LAST(true);
                # Rest durch '...' abkürzen:
                write_ascii_char(stream_,'.');
                write_ascii_char(stream_,'.');
                write_ascii_char(stream_,'.');
                skipSTACK(1); # slot vergessen
                break;
              }
              JUSTIFY_LAST(!some_printable_slots(STACK_1));
              var object* slot_ = &STACK_0; # da sitzt der Slot
              JUSTIFY_START(0);
              JUSTIFY_LAST(false);
              write_ascii_char(stream_,':'); # Keyword-Kennzeichen
              {
                var object obj = TheSvector(*slot_)->data[0]; # (ds-slot-name slot)
                if (!symbolp(obj)) goto bad_description; # sollte ein Symbol sein
                pr_like_symbol(stream_,Symbol_name(obj)); # Symbolnamen der Komponente ausgeben
              }
              JUSTIFY_SPACE;
              JUSTIFY_LAST(true);
              # (SYS::%%STRUCTURE-REF name Structure (ds-slot-offset slot)) ausführen:
              pushSTACK(*(structure_ STACKop -1)); # name als 1. Argument
              pushSTACK(*(structure_ STACKop 0)); # Structure als 2. Argument
              pushSTACK(TheSvector(*slot_)->data[2]); # (ds-slot-offset slot) als 3. Argument
              funcall(L(pstructure_ref),3);
              prin_object(stream_,value1); # Komponente ausgeben
              JUSTIFY_END_ENG;
              skipSTACK(1); # slot vergessen
            }
          }
        }
        skipSTACK(1);
        JUSTIFY_END_ENG;
        if (readable) { # Beendigung der Fallunterscheidung von oben
          INDENT_END;
          KLAMMER_ZU;
        } else {
          INDENT_END;
          write_ascii_char(stream_,'>');
        }
        skipSTACK(3);
      } else {
        # Structure elementweise, ohne Komponenten-Namen ausgeben.
        if (test_value(S(print_readably)))
          fehler_print_readably(*structure_);
        write_ascii_char(stream_,'#'); write_ascii_char(stream_,'<');
        INDENT_START(2); # um 2 Zeichen einrücken, wegen '#<'
        JUSTIFY_START(1);
        var uintC len = Structure_length(*structure_); # Länge der Structure (>=1)
        JUSTIFY_LAST(len==1);
        prin_object(stream_,*(structure_ STACKop -1)); # name ausgeben
        var uintL length_limit = get_print_length(); # *PRINT-LENGTH*-Begrenzung
        var uintL length = 0; # Index = bisherige Länge := 0
        for (len = len-1; len > 0; len--) {
          JUSTIFY_SPACE; # Space ausgeben
          # auf Erreichen von *PRINT-LENGTH* prüfen:
          if (length >= length_limit) {
            # Rest durch '...' abkürzen:
            JUSTIFY_LAST(true);
            write_ascii_char(stream_,'.');
            write_ascii_char(stream_,'.');
            write_ascii_char(stream_,'.');
            break;
          }
          JUSTIFY_LAST(len==1);
          length++; # Index erhöhen
          # Komponente ausgeben:
          prin_object(stream_,TheStructure(*structure_)->recdata[length]);
        }
        JUSTIFY_END_ENG;
        INDENT_END;
        write_ascii_char(stream_,'>');
        skipSTACK(2);
      }
    }

# Das ist die Default-Funktion, die von CLOS:PRINT-OBJECT aufgerufen wird:
LISPFUNN(print_structure,2)
  {
    # stack layout: structure, stream.
    var object structure = STACK_1;
    if (!structurep(structure)) {
      pushSTACK(structure);           # TYPE-ERROR slot DATUM
      pushSTACK(S(structure_object)); # TYPE-ERROR slot EXPECTED-TYPE
      pushSTACK(structure); # structure
      pushSTACK(TheSubr(subr_self)->name); # function name
      fehler(type_error,
             GETTEXT("~: ~ is not a structure")
            );
    }
    if (!streamp(STACK_0))
      fehler_stream(STACK_0);
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
    {
      pushSTACK(string); # String retten
      var object* string_ = &STACK_0; # und merken, wo er sitzt
      write_ascii_char(stream_,'#'); write_ascii_char(stream_,'<');
      INDENT_START(2); # um 2 Zeichen einrücken, wegen '#<'
      JUSTIFY_START(1);
      JUSTIFY_LAST(false);
      write_sstring_case(stream_,*string_); # String ausgeben
      JUSTIFY_SPACE;
      JUSTIFY_LAST(true);
      pr_hex6(stream_,obj); # obj als Adresse ausgeben
      JUSTIFY_END_ENG;
      INDENT_END;
      write_ascii_char(stream_,'>');
      skipSTACK(1);
    }

# UP: Gibt einen Maschinenpointer auf einen Stream aus.
# pr_machine(&stream,obj);
# > obj: Maschinenpointer
# > stream: Stream
# < stream: Stream
# can trigger GC
  local void pr_machine(stream_,obj)
    var const object* stream_;
    var object obj;
    {
      # #<ADDRESS #x...>
      if (test_value(S(print_readably)))
        fehler_print_readably(obj);
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
    {
      if (test_value(S(print_readably)))
        fehler_print_readably(obj);
      if (eq(obj,unbound)) { # #<UNBOUND>
        write_sstring_case(stream_,O(printstring_unbound));
      } elif (eq(obj,specdecl)) { # #<SPECIAL REFERENCE>
        write_sstring_case(stream_,O(printstring_special_reference));
      } elif (eq(obj,disabled)) { # #<DISABLED POINTER>
        write_sstring_case(stream_,O(printstring_disabled_pointer));
      } elif (eq(obj,dot_value)) { # #<DOT>
        write_sstring_case(stream_,O(printstring_dot));
      } elif (eq(obj,eof_value)) { # #<END OF FILE>
        write_sstring_case(stream_,O(printstring_eof));
      } else { # #<SYSTEM-POINTER #x...>
        pr_hex6_obj(stream_,obj,O(printstring_system));
      }
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
    {
      if (test_value(S(print_readably)))
        fehler_print_readably(obj);
      # #<READ-LABEL ...>
      write_ascii_char(stream_,'#'); write_ascii_char(stream_,'<');
      INDENT_START(2); # um 2 Zeichen einrücken, wegen '#<'
      JUSTIFY_START(1);
      JUSTIFY_LAST(false);
      write_sstring_case(stream_,O(printstring_read_label)); # "READ-LABEL"
      JUSTIFY_SPACE;
      JUSTIFY_LAST(true);
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
    {
      if (test_value(S(print_readably)))
        fehler_print_readably(obj);
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
    {
      var uintL len = Record_length(*obj_); # Länge des Record
      var uintL length_limit = get_print_length(); # *PRINT-LENGTH*-Begrenzung
      loop {
        if (index >= len) break; # Index >= Recordlänge -> fertig
        JUSTIFY_SPACE; # Space ausgeben
        # auf Erreichen von *PRINT-LENGTH* prüfen:
        if (length >= length_limit) {
          JUSTIFY_LAST(true);
          # Rest durch '...' abkürzen:
          write_ascii_char(stream_,'.');
          write_ascii_char(stream_,'.');
          write_ascii_char(stream_,'.');
          break;
        }
        JUSTIFY_LAST(index+1 >= len);
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
    {
      var uintL length_limit = get_print_length(); # *PRINT-LENGTH*-Begrenzung
      pushSTACK(obj);
      while (mconsp(STACK_0)) {
        JUSTIFY_SPACE; # Space ausgeben
        # auf Erreichen von *PRINT-LENGTH* prüfen:
        if (length >= length_limit) {
          JUSTIFY_LAST(true);
          # Rest durch '...' abkürzen:
          write_ascii_char(stream_,'.');
          write_ascii_char(stream_,'.');
          write_ascii_char(stream_,'.');
          break;
        }
        {
          var object list = STACK_0;
          STACK_0 = Cdr(list); # Liste verkürzen
          JUSTIFY_LAST(matomp(STACK_0));
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
  local void pr_record_descr (const object* stream_, object obj, object name, bool readable, object slotlist);
  local void pr_record_descr(stream_,obj,name,readable,slotlist)
    var const object* stream_;
    var object obj;
    var object name;
    var bool readable;
    var object slotlist;
    {
      LEVEL_CHECK;
      {
        pushSTACK(obj);
        pushSTACK(name);
        pushSTACK(slotlist);
        # stack layout: obj, name, slotlist.
        var object* obj_ = &STACK_2;
        # Es ist *(obj_ STACKop 0) = obj
        # und    *(obj_ STACKop -1) = name
        # und    *(obj_ STACKop -2) = slotlist .
        if (readable) {
          # obj wiedereinlesbar ausgeben:
          write_ascii_char(stream_,'#'); write_ascii_char(stream_,'S');
          KLAMMER_AUF;
          INDENT_START(3); # um 3 Zeichen einrücken, wegen '#S('
        } else {
          # obj nicht wiedereinlesbar ausgeben:
          if (test_value(S(print_readably)))
            fehler_print_readably(STACK_2);
          write_ascii_char(stream_,'#'); write_ascii_char(stream_,'<');
          INDENT_START(2); # um 2 Zeichen einrücken, wegen '#<'
        }
        JUSTIFY_START(1);
        pushSTACK(*(obj_ STACKop -2));
        JUSTIFY_LAST(matomp(STACK_0));
        prin_object(stream_,*(obj_ STACKop -1)); # name ausgeben
        # Slot-Liste STACK_0 = (svref description 3) durchlaufen:
        {
          var uintL length_limit = get_print_length(); # *PRINT-LENGTH*-Begrenzung
          var uintL length = 0; # bisherige Länge := 0
          while (mconsp(STACK_0)) {
            {
              var object slotlistr = STACK_0;
              STACK_0 = Cdr(slotlistr); # Liste verkürzen
              pushSTACK(Car(slotlistr)); # ein einzelner slot
            }
            JUSTIFY_SPACE; # Space ausgeben
            # auf Erreichen von *PRINT-LENGTH* prüfen:
            if (length >= length_limit) {
              # Rest durch '...' abkürzen:
              JUSTIFY_LAST(true);
              write_ascii_char(stream_,'.');
              write_ascii_char(stream_,'.');
              write_ascii_char(stream_,'.');
              skipSTACK(1); # slot vergessen
              break;
            }
            JUSTIFY_LAST(matomp(STACK_1));
            var object* slot_ = &STACK_0; # da sitzt der Slot
            JUSTIFY_START(0);
            JUSTIFY_LAST(false);
            write_ascii_char(stream_,':'); # Keyword-Kennzeichen
            # (first slot) sollte ein Symbol sein
            pr_like_symbol(stream_,Symbol_name(Car(*slot_))); # Symbolnamen der Komponente ausgeben
            JUSTIFY_SPACE;
            JUSTIFY_LAST(true);
            pushSTACK(*(obj_ STACKop 0)); # obj als Argument
            funcall(Cdr(*slot_),1); # accessor aufrufen
            prin_object(stream_,value1); # Komponente ausgeben
            JUSTIFY_END_ENG;
            skipSTACK(1); # slot vergessen
          }
        }
        skipSTACK(1);
        JUSTIFY_END_ENG;
        if (readable) { # Beendigung der Fallunterscheidung von oben
          INDENT_END;
          KLAMMER_ZU;
        } else {
          INDENT_END;
          write_ascii_char(stream_,'>');
        }
        skipSTACK(3);
      }
      LEVEL_END;
    }

# UP: Gibt einen OtherRecord auf einen Stream aus.
# pr_orecord(&stream,obj);
# > obj: OtherRecord
# > stream: Stream
# < stream: Stream
# can trigger GC
  local void pr_orecord(stream_,obj)
    var const object* stream_;
    var object obj;
    {
      switch (Record_type(obj)) {
        #ifndef TYPECODES
        case Rectype_string: case Rectype_Sstring: case Rectype_Imm_Sstring: case Rectype_Imm_SmallSstring: # String
          pr_string(stream_,obj); break;
        case Rectype_bvector: case Rectype_Sbvector: # Bit-Vektor
          pr_bvector(stream_,obj); break;
        case Rectype_b2vector: case Rectype_Sb2vector: # 2Bit-Vektor
        case Rectype_b4vector: case Rectype_Sb4vector: # 4Bit-Vektor
        case Rectype_b8vector: case Rectype_Sb8vector: # 8Bit-Vektor
        case Rectype_b16vector: case Rectype_Sb16vector: # 16Bit-Vektor
        case Rectype_b32vector: case Rectype_Sb32vector: # 32Bit-Vektor
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
          if (test_value(S(print_array)) || test_value(S(print_readably))) {
            LEVEL_CHECK;
            {
              pushSTACK(obj); # Hash-Tabelle retten
              var object* obj_ = &STACK_0; # und merken, wo sie sitzt
              write_ascii_char(stream_,'#'); write_ascii_char(stream_,'S');
              KLAMMER_AUF;
              INDENT_START(3); # um 3 Zeichen einrücken, wegen '#S('
              JUSTIFY_START(1);
              JUSTIFY_LAST(false);
              prin_object(stream_,S(hash_table)); # Symbol HASH-TABLE ausgeben
              obj = *obj_;
              {
                var uintL count = posfixnum_to_L(TheHashtable(*obj_)->ht_count);
                var uintL index = # Index in den Key-Value-Vektor
                  2*posfixnum_to_L(TheHashtable(obj)->ht_maxcount);
                pushSTACK(TheHashtable(obj)->ht_kvtable); # Key-Value-Vektor
                var uintL length_limit = get_print_length(); # *PRINT-LENGTH*-Begrenzung
                var uintL length = 0; # bisherige Länge := 0
                JUSTIFY_SPACE; # Space ausgeben
                # auf Erreichen von *PRINT-LENGTH* prüfen:
                if (length >= length_limit)
                  goto dots;
                JUSTIFY_LAST(count==0);
                # Hash-Test ausgeben:
                {
                  var uintB flags = record_flags(TheHashtable(*obj_));
                  var object test = # Test-Symbol EQ/EQL/EQUAL
                    (flags & bit(0) ? S(eq) :
                     flags & bit(1) ? S(eql) :
                     flags & bit(2) ? S(equal) :
                                      NIL # (Default-Symbol)
                    );
                  prin_object(stream_,test);
                }
                loop {
                  length++; # bisherige Länge erhöhen
                  # nächstes auszugebendes Key-Value-Paar suchen:
                  loop {
                    if (index==0) # kvtable zu Ende?
                      goto kvtable_end;
                    index -= 2; # Index verringern
                    if (!eq(TheSvector(STACK_0)->data[index+0],unbound)) # Key /= "leer" ?
                      break;
                  }
                  JUSTIFY_SPACE; # Space ausgeben
                  # auf Erreichen von *PRINT-LENGTH* prüfen:
                  if (length >= length_limit) {
                   dots:
                    JUSTIFY_LAST(true);
                    # Rest durch '...' abkürzen:
                    write_ascii_char(stream_,'.');
                    write_ascii_char(stream_,'.');
                    write_ascii_char(stream_,'.');
                    break;
                  }
                  count--;
                  JUSTIFY_LAST(count==0);
                  # Cons (Key . Value) bilden und ausgeben:
                  obj = allocate_cons();
                  {
                    var object* ptr = &TheSvector(STACK_0)->data[index];
                    Car(obj) = ptr[0]; # Key
                    Cdr(obj) = ptr[1]; # Value
                  }
                  prin_object(stream_,obj);
                }
               kvtable_end: # Ende der Ausgabe der Key-Value-Paare
                skipSTACK(1);
              }
              JUSTIFY_END_ENG;
              INDENT_END;
              KLAMMER_ZU;
              skipSTACK(1);
            }
            LEVEL_END;
          } else {
            pr_hex6_obj(stream_,obj,O(printstring_hash_table));
          }
          break;
        case Rectype_Package:
          # je nach *PRINT-READABLY*:
          # #<PACKAGE name> oder #.(SYSTEM::%FIND-PACKAGE "name")
          {
            pushSTACK(obj); # Package retten
            var object* obj_ = &STACK_0; # und merken, wo sie sitzt
            if (!test_value(S(print_readably))) {
              write_ascii_char(stream_,'#'); write_ascii_char(stream_,'<');
              INDENT_START(2); # um 2 Zeichen einrücken, wegen '#<'
              JUSTIFY_START(1);
              JUSTIFY_LAST(false);
              if (pack_deletedp(*obj_))
                write_sstring_case(stream_,O(printstring_deleted)); # "DELETED "
              write_sstring_case(stream_,O(printstring_package)); # "PACKAGE"
              JUSTIFY_SPACE;
              JUSTIFY_LAST(true);
              pr_like_symbol(stream_,ThePackage(*obj_)->pack_name); # Name ausgeben
              JUSTIFY_END_ENG;
              INDENT_END;
              write_ascii_char(stream_,'>');
            } else {
              if (!(test_value(S(read_eval)) || stream_get_read_eval(*stream_)))
                fehler_print_readably(*obj_);
              if (pack_deletedp(*obj_))
                fehler_print_readably(*obj_);
              write_ascii_char(stream_,'#'); write_ascii_char(stream_,'.');
              KLAMMER_AUF; # '('
              INDENT_START(3); # um 3 Zeichen einrücken, wegen '#.('
              JUSTIFY_START(1);
              JUSTIFY_LAST(false);
              pr_symbol(stream_,S(pfind_package)); # SYSTEM::%FIND-PACKAGE
              JUSTIFY_SPACE;
              JUSTIFY_LAST(true);
              pr_string(stream_,ThePackage(*obj_)->pack_name); # Name ausgeben
              JUSTIFY_END_ENG;
              INDENT_END;
              KLAMMER_ZU;
            }
            skipSTACK(1);
          }
          break;
        case Rectype_Readtable:
          # #<READTABLE #x...>
          if (test_value(S(print_readably)))
            fehler_print_readably(obj);
          pr_hex6_obj(stream_,obj,O(printstring_readtable));
          break;
        case Rectype_Pathname:
          pushSTACK(obj); # pathname
          # call (NAMESTRING pathname)
          pushSTACK(obj); funcall(L(namestring),1); obj = value1;
          ASSERT(stringp(obj));
          if (test_value(S(print_readably))
              && !test_value(S(print_pathnames_ansi))) {
            pushSTACK(obj); # string
            var object* obj_ = &STACK_0;
            JUSTIFY_START(0);
            JUSTIFY_LAST(false);
            {
              JUSTIFY_START(0);
              JUSTIFY_LAST(false);
              write_ascii_char(stream_,'#'); write_ascii_char(stream_,'-');
              write_sstring(stream_,O(lisp_implementation_type_string));
              JUSTIFY_SPACE;
              JUSTIFY_LAST(true);
              write_ascii_char(stream_,'#'); write_ascii_char(stream_,'P');
              pr_string(stream_,*obj_);
              JUSTIFY_END_ENG;
            }
            JUSTIFY_SPACE;
            JUSTIFY_LAST(true);
            {
              JUSTIFY_START(0);
              JUSTIFY_LAST(false);
              write_ascii_char(stream_,'#'); write_ascii_char(stream_,'+');
              write_sstring(stream_,O(lisp_implementation_type_string));
              JUSTIFY_SPACE;
              JUSTIFY_LAST(true);
              pr_record_descr(stream_,*(obj_ STACKop 1),S(pathname),true,
                              O(pathname_slotlist));
              JUSTIFY_END_ENG;
            }
            JUSTIFY_END_ENG;
            skipSTACK(1);
          } else {
            STACK_0 = obj; # String
            if (test_value(S(print_escape)) || test_value(S(print_readably))) {
              # print "#P"
              write_ascii_char(stream_,'#'); write_ascii_char(stream_,'P');
            }
            pr_string(stream_,STACK_0); # print the string
          }
          skipSTACK(1);
          break;
        #ifdef LOGICAL_PATHNAMES
        case Rectype_Logpathname:
          # #S(LOGICAL-PATHNAME :HOST host :DIRECTORY directory :NAME name :TYPE type :VERSION version)
          pr_record_descr(stream_,obj,S(logical_pathname),true,O(pathname_slotlist));
          break;
        #endif
        case Rectype_Random_State:
          # #S(RANDOM-STATE seed)
          LEVEL_CHECK;
          {
            pushSTACK(obj); # Random-State retten
            var object* obj_ = &STACK_0; # und merken, wo es sitzt
            write_ascii_char(stream_,'#'); write_ascii_char(stream_,'S');
            KLAMMER_AUF;
            INDENT_START(3); # um 3 Zeichen einrücken, wegen '#S('
            JUSTIFY_START(1);
            JUSTIFY_LAST(false);
            prin_object(stream_,S(random_state)); # Symbol RANDOM-STATE ausgeben
            pr_record_ab(stream_,obj_,0,0); # Komponente ausgeben
            JUSTIFY_END_ENG;
            INDENT_END;
            KLAMMER_ZU;
            skipSTACK(1);
          }
          LEVEL_END;
          break;
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
          if (test_value(S(print_readably)))
            fehler_print_readably(obj);
          LEVEL_CHECK;
          {
            pushSTACK(obj); # Byte retten
            var object* obj_ = &STACK_0; # und merken, wo es sitzt
            write_ascii_char(stream_,'#'); write_ascii_char(stream_,'<');
            INDENT_START(2); # um 2 Zeichen einrücken, wegen '#<'
            JUSTIFY_START(1);
            JUSTIFY_LAST(false);
            write_sstring_case(stream_,O(printstring_byte)); # "BYTE"
            pr_record_ab(stream_,obj_,0,0); # Komponenten ausgeben
            JUSTIFY_END_ENG;
            INDENT_END;
            write_ascii_char(stream_,'>');
            skipSTACK(1);
          }
          LEVEL_END;
          #else
          # #S(BYTE :SIZE size :POSITION position)
          pr_record_descr(stream_,obj,S(byte),true,O(byte_slotlist));
          #endif
          break;
        case Rectype_Fsubr: # Fsubr
          pr_fsubr(stream_,obj);
          break;
        case Rectype_Loadtimeeval:
          # #.form
          if (test_value(S(print_readably)))
            if (!(test_value(S(read_eval)) || stream_get_read_eval(*stream_)))
              fehler_print_readably(obj);
          pushSTACK(TheLoadtimeeval(obj)->loadtimeeval_form); # form retten
          write_ascii_char(stream_,'#'); write_ascii_char(stream_,'.');
          obj = popSTACK();
          INDENT_START(2); # um 2 Zeichen einrücken, wegen '#.'
          prin_object(stream_,obj); # form ausgeben
          INDENT_END;
          break;
        case Rectype_Symbolmacro:
          # #<SYMBOL-MACRO expansion>
          if (test_value(S(print_readably)))
            fehler_print_readably(obj);
          LEVEL_CHECK;
          {
            pushSTACK(obj); # Symbol-Macro retten
            var object* obj_ = &STACK_0; # und merken, wo es sitzt
            write_ascii_char(stream_,'#'); write_ascii_char(stream_,'<');
            INDENT_START(2); # um 2 Zeichen einrücken, wegen '#<'
            JUSTIFY_START(1);
            JUSTIFY_LAST(false);
            write_sstring_case(stream_,O(printstring_symbolmacro)); # "SYMBOL-MACRO"
            pr_record_ab(stream_,obj_,0,0); # Komponente ausgeben
            JUSTIFY_END_ENG;
            INDENT_END;
            write_ascii_char(stream_,'>');
            skipSTACK(1);
          }
          LEVEL_END;
          break;
        case Rectype_Macro:
          # #<MACRO expansion>
          if (test_value(S(print_readably)))
            fehler_print_readably(obj);
          LEVEL_CHECK;
          {
            pushSTACK(obj); # Macro retten
            var object* obj_ = &STACK_0; # und merken, wo es sitzt
            write_ascii_char(stream_,'#'); write_ascii_char(stream_,'<');
            INDENT_START(2); # um 2 Zeichen einrücken, wegen '#<'
            JUSTIFY_START(1);
            JUSTIFY_LAST(false);
            write_sstring_case(stream_,O(printstring_macro)); # "MACRO"
            pr_record_ab(stream_,obj_,0,0); # Komponente ausgeben
            JUSTIFY_END_ENG;
            INDENT_END;
            write_ascii_char(stream_,'>');
            skipSTACK(1);
          }
          LEVEL_END;
          break;
        case Rectype_FunctionMacro:
          # #<FUNCTION-MACRO expansion>
          if (test_value(S(print_readably)))
            fehler_print_readably(obj);
          LEVEL_CHECK;
          {
            pushSTACK(obj); # FunctionMacro retten
            var object* obj_ = &STACK_0; # und merken, wo es sitzt
            write_ascii_char(stream_,'#'); write_ascii_char(stream_,'<');
            INDENT_START(2); # um 2 Zeichen einrücken, wegen '#<'
            JUSTIFY_START(1);
            JUSTIFY_LAST(false);
            write_sstring_case(stream_,O(printstring_functionmacro)); # "FUNCTION-MACRO"
            pr_record_ab(stream_,obj_,0,0); # Komponente ausgeben
            JUSTIFY_END_ENG;
            INDENT_END;
            write_ascii_char(stream_,'>');
            skipSTACK(1);
          }
          LEVEL_END;
          break;
        case Rectype_Encoding:
          # #<ENCODING [charset] line-terminator>
          if (test_value(S(print_readably)))
            fehler_print_readably(obj);
          LEVEL_CHECK;
          {
            pushSTACK(obj); # Encoding retten
            var object* obj_ = &STACK_0; # und merken, wo es sitzt
            write_ascii_char(stream_,'#'); write_ascii_char(stream_,'<');
            INDENT_START(2); # um 2 Zeichen einrücken, wegen '#<'
            JUSTIFY_START(1);
            var uintL length_limit = get_print_length(); # *PRINT-LENGTH*
            #ifdef UNICODE
            JUSTIFY_LAST(length_limit==0);
            #else
            JUSTIFY_LAST(true);
            #endif
            write_sstring_case(stream_,O(printstring_encoding)); # "ENCODING"
            {
              var uintL length = 0; # bisherige Länge := 0
              #ifdef UNICODE
              # auf Erreichen von *PRINT-LENGTH* prüfen:
              if (length >= length_limit) goto encoding_end;
              JUSTIFY_SPACE; # Space ausgeben
              JUSTIFY_LAST(length+1 >= length_limit);
              # Charset ausgeben:
              prin_object(stream_,TheEncoding(*obj_)->enc_charset);
              length++; # bisherige Länge erhöhen
              #endif
              # auf Erreichen von *PRINT-LENGTH* prüfen:
              if (length >= length_limit) goto encoding_end;
              JUSTIFY_SPACE; # Space ausgeben
              JUSTIFY_LAST(true);
              # Line-Terminator ausgeben:
              prin_object(stream_,TheEncoding(*obj_)->enc_eol);
              length++; # bisherige Länge erhöhen
            }
           encoding_end:
            JUSTIFY_END_ENG;
            INDENT_END;
            write_ascii_char(stream_,'>');
            skipSTACK(1);
          }
          LEVEL_END;
          break;
        #ifdef FOREIGN
        case Rectype_Fpointer:
          # #<FOREIGN-POINTER address>
          if (test_value(S(print_readably)))
            fehler_print_readably(obj);
          LEVEL_CHECK;
          {
            var bool validp = fp_validp(TheFpointer(obj));
            var uintP val = (uintP)(TheFpointer(obj)->fp_pointer); # Wert holen
            write_ascii_char(stream_,'#'); write_ascii_char(stream_,'<');
            INDENT_START(2); # um 2 Zeichen einrücken, wegen '#<'
            JUSTIFY_START(1);
            var uintL length_limit = get_print_length(); # *PRINT-LENGTH*
            JUSTIFY_LAST(length_limit==0);
            if (!validp)
              write_sstring_case(stream_,O(printstring_invalid)); # "INVALID "
            write_sstring_case(stream_,O(printstring_fpointer)); # "FOREIGN-POINTER"
            {
              var uintL length = 0; # bisherige Länge := 0
              # auf Erreichen von *PRINT-LENGTH* prüfen:
              if (length >= length_limit) goto fpointer_end;
              JUSTIFY_SPACE; # Space ausgeben
              JUSTIFY_LAST(true);
              # Adresse ausgeben:
              pr_hex8(stream_,val);
              length++; # bisherige Länge erhöhen
            }
           fpointer_end:
            JUSTIFY_END_ENG;
            INDENT_END;
            write_ascii_char(stream_,'>');
          }
          LEVEL_END;
          break;
        #endif
        #ifdef DYNAMIC_FFI
        case Rectype_Faddress:
          # #<FOREIGN-ADDRESS #x...>
          if (test_value(S(print_readably)))
            fehler_print_readably(obj);
          LEVEL_CHECK;
          {
            pushSTACK(obj); # retten
            var object* obj_ = &STACK_0; # und merken, wo es sitzt
            write_ascii_char(stream_,'#'); write_ascii_char(stream_,'<');
            INDENT_START(2); # um 2 Zeichen einrücken, wegen '#<'
            JUSTIFY_START(1);
            var uintL length_limit = get_print_length(); # *PRINT-LENGTH*
            JUSTIFY_LAST(length_limit==0);
            if (!fp_validp(TheFpointer(TheFaddress(*obj_)->fa_base)))
              write_sstring_case(stream_,O(printstring_invalid)); # "INVALID "
            write_sstring_case(stream_,O(printstring_faddress)); # "FOREIGN-ADDRESS"
            {
              var uintL length = 0; # bisherige Länge := 0
              # auf Erreichen von *PRINT-LENGTH* prüfen:
              if (length >= length_limit) goto faddress_end;
              JUSTIFY_SPACE; # Space ausgeben
              JUSTIFY_LAST(true);
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
          }
          LEVEL_END;
          break;
        case Rectype_Fvariable:
          # #<FOREIGN-VARIABLE name #x...>
          if (test_value(S(print_readably)))
            fehler_print_readably(obj);
          LEVEL_CHECK;
          {
            pushSTACK(obj); # retten
            var object* obj_ = &STACK_0; # und merken, wo es sitzt
            write_ascii_char(stream_,'#'); write_ascii_char(stream_,'<');
            INDENT_START(2); # um 2 Zeichen einrücken, wegen '#<'
            JUSTIFY_START(1);
            var uintL length_limit = get_print_length(); # *PRINT-LENGTH*
            JUSTIFY_LAST(length_limit==0);
            write_sstring_case(stream_,O(printstring_fvariable)); # "FOREIGN-VARIABLE"
            {
              var uintL length = 0; # bisherige Länge := 0
              # auf Erreichen von *PRINT-LENGTH* prüfen:
              if (length >= length_limit) goto fvariable_end;
              JUSTIFY_SPACE; # Space ausgeben
              # Name ausgeben:
              if (!nullp(TheFvariable(*obj_)->fv_name)) {
                JUSTIFY_LAST(length+1 >= length_limit);
                prin_object(stream_,TheFvariable(*obj_)->fv_name);
                length++; # bisherige Länge erhöhen
                if (length >= length_limit) goto fvariable_end;
                JUSTIFY_SPACE; # Space ausgeben
              }
              JUSTIFY_LAST(true);
              # Adresse ausgeben:
              pr_hex8(stream_,(uintP)Faddress_value(TheFvariable(*obj_)->fv_address));
              length++; # bisherige Länge erhöhen
            }
           fvariable_end:
            JUSTIFY_END_ENG;
            INDENT_END;
            write_ascii_char(stream_,'>');
            skipSTACK(1);
          }
          LEVEL_END;
          break;
        case Rectype_Ffunction:
          # #<FOREIGN-FUNCTION name #x...>
          if (test_value(S(print_readably)))
            fehler_print_readably(obj);
          LEVEL_CHECK;
          {
            pushSTACK(obj); # retten
            var object* obj_ = &STACK_0; # und merken, wo es sitzt
            write_ascii_char(stream_,'#'); write_ascii_char(stream_,'<');
            INDENT_START(2); # um 2 Zeichen einrücken, wegen '#<'
            JUSTIFY_START(1);
            var uintL length_limit = get_print_length(); # *PRINT-LENGTH*
            JUSTIFY_LAST(length_limit==0);
            write_sstring_case(stream_,O(printstring_ffunction)); # "FOREIGN-FUNCTION"
            {
              var uintL length = 0; # bisherige Länge := 0
              # auf Erreichen von *PRINT-LENGTH* prüfen:
              if (length >= length_limit) goto ffunction_end;
              JUSTIFY_SPACE; # Space ausgeben
              # Name ausgeben:
              if (!nullp(TheFfunction(*obj_)->ff_name)) {
                JUSTIFY_LAST(length+1 >= length_limit);
                prin_object(stream_,TheFfunction(*obj_)->ff_name);
                length++; # bisherige Länge erhöhen
                if (length >= length_limit) goto ffunction_end;
                JUSTIFY_SPACE; # Space ausgeben
              }
              JUSTIFY_LAST(true);
              # Adresse ausgeben:
              pr_hex8(stream_,(uintP)Faddress_value(TheFfunction(*obj_)->ff_address));
              length++; # bisherige Länge erhöhen
            }
           ffunction_end:
            JUSTIFY_END_ENG;
            INDENT_END;
            write_ascii_char(stream_,'>');
            skipSTACK(1);
          }
          LEVEL_END;
          break;
        #endif
        case Rectype_Weakpointer:
          # #<WEAK-POINTER value> or #<BROKEN WEAK-POINTER>
          if (test_value(S(print_readably)))
            fehler_print_readably(obj);
          if (!eq(TheWeakpointer(obj)->wp_cdr,unbound)) {
            LEVEL_CHECK;
            {
              pushSTACK(TheWeakpointer(obj)->wp_value); # value retten
              var object* value_ = &STACK_0; # und merken, wo es sitzt
              write_ascii_char(stream_,'#'); write_ascii_char(stream_,'<');
              INDENT_START(2); # um 2 Zeichen einrücken, wegen '#<'
              JUSTIFY_START(1);
              var uintL length_limit = get_print_length(); # *PRINT-LENGTH*
              JUSTIFY_LAST(length_limit==0);
              write_sstring_case(stream_,O(printstring_weakpointer)); # "WEAK-POINTER"
              {
                var uintL length = 0; # bisherige Länge := 0
                # auf Erreichen von *PRINT-LENGTH* prüfen:
                if (length >= length_limit) goto weakpointer_end;
                JUSTIFY_SPACE; # Space ausgeben
                JUSTIFY_LAST(true);
                prin_object(stream_,*value_); # output value
                length++; # bisherige Länge erhöhen
              }
             weakpointer_end:
              JUSTIFY_END_ENG;
              INDENT_END;
              write_ascii_char(stream_,'>');
              skipSTACK(1);
            }
            LEVEL_END;
          } else {
            write_sstring_case(stream_,O(printstring_broken_weakpointer));
          }
          break;
        case Rectype_Finalizer:
          # #<FINALIZER>
          if (test_value(S(print_readably)))
            fehler_print_readably(obj);
          write_sstring_case(stream_,O(printstring_finalizer));
          break;
        #ifdef SOCKET_STREAMS
        case Rectype_Socket_Server:
          # #<SOCKET-SERVER host:port>
          if (test_value(S(print_readably)))
            fehler_print_readably(obj);
          LEVEL_CHECK;
          {
            pushSTACK(obj); # retten
            var object* obj_ = &STACK_0; # und merken, wo es sitzt
            write_ascii_char(stream_,'#'); write_ascii_char(stream_,'<');
            INDENT_START(2); # um 2 Zeichen einrücken, wegen '#<'
            JUSTIFY_START(1);
            var uintL length_limit = get_print_length(); # *PRINT-LENGTH*
            JUSTIFY_LAST(length_limit==0);
            # falls geschlossen, "CLOSED " ausgeben:
            if (nullp(TheSocketServer(*obj_)->socket_handle))
              write_sstring_case(stream_,O(printstring_closed));
            write_sstring_case(stream_,O(printstring_socket_server)); # "SOCKET-SERVER"
            {
              var uintL length = 0; # bisherige Länge := 0
              # auf Erreichen von *PRINT-LENGTH* prüfen:
              if (length >= length_limit) goto socket_server_end;
              JUSTIFY_SPACE; # Space ausgeben
              JUSTIFY_LAST(true);
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
          }
          LEVEL_END;
          break;
        #endif
        #ifdef DIR_KEY
        case Rectype_Dir_Key:
          # #<DIR-KEY type path>
          if (test_value(S(print_readably)))
            fehler_print_readably(obj);
          LEVEL_CHECK;
          {
            pushSTACK(obj);
            var object* obj_ = &STACK_0;
            write_ascii_char(stream_,'#'); write_ascii_char(stream_,'<');
            INDENT_START(2);
            JUSTIFY_START(1);
            var uintL length_limit = get_print_length();
            JUSTIFY_LAST(length_limit==0);
            if (TheDirKey(*obj_)->closed_p)
              write_sstring_case(stream_,O(printstring_closed));
            write_sstring_case(stream_,O(printstring_dir_key));
            {
              var uintL length = 0;
              if (length >= length_limit) goto dir_key_end;
              JUSTIFY_SPACE;
              JUSTIFY_LAST(length+1 >= length_limit);
              pr_symbol(stream_,TheDirKey(*obj_)->type);
              length++;
              if (length >= length_limit) goto dir_key_end;
              JUSTIFY_SPACE;
              JUSTIFY_LAST(TheDirKey(*obj_)->closed_p || (length+1 >= length_limit));
              pr_string(stream_,TheDirKey(*obj_)->path);
              if (!TheDirKey(*obj_)->closed_p) {
                length++;
                if (length >= length_limit) goto dir_key_end;
                JUSTIFY_SPACE;
                JUSTIFY_LAST(true);
                pr_symbol(stream_,TheDirKey(*obj_)->direction);
              }
            }
          dir_key_end:
            JUSTIFY_END_ENG;
            INDENT_END;
            write_ascii_char(stream_,'>');
            skipSTACK(1);
          }
          LEVEL_END;
          break;
        #endif
        #ifdef YET_ANOTHER_RECORD
        case Rectype_Yetanother:
          # #<YET-ANOTHER address>
          if (test_value(S(print_readably)))
            fehler_print_readably(obj);
          LEVEL_CHECK;
          {
            pushSTACK(obj); # Yetanother retten
            var object* obj_ = &STACK_0; # und merken, wo es sitzt
            write_ascii_char(stream_,'#'); write_ascii_char(stream_,'<');
            INDENT_START(2); # um 2 Zeichen einrücken, wegen '#<'
            JUSTIFY_START(1);
            var uintL length_limit = get_print_length(); # *PRINT-LENGTH*-Begrenzung
            JUSTIFY_LAST(length_limit==0);
            write_sstring_case(stream_,O(printstring_yetanother)); # "YET-ANOTHER"
            {
              var uintL length = 0; # bisherige Länge := 0
              # auf Erreichen von *PRINT-LENGTH* prüfen:
              if (length >= length_limit) goto yetanother_end;
              JUSTIFY_SPACE; # Space ausgeben
              JUSTIFY_LAST(true);
              # x ausgeben:
              pr_hex6(stream_,TheYetanother(*obj_)->yetanother_x);
              length++; # bisherige Länge erhöhen
            }
           yetanother_end:
            JUSTIFY_END_ENG;
            INDENT_END;
            write_ascii_char(stream_,'>');
            skipSTACK(1);
          }
          LEVEL_END;
          break;
        #endif
        default:
          pushSTACK(S(print));
          fehler(serious_condition,
                 GETTEXT("~: an unknown record type has been generated!")
                );
      }
    }

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
    {
      pushSTACK(other); # other retten
      pushSTACK(string); # String retten
      var object* string_ = &STACK_0; # und merken, wo beides sitzt
      write_ascii_char(stream_,'#'); write_ascii_char(stream_,'<');
      INDENT_START(2); # um 2 Zeichen einrücken, wegen '#<'
      JUSTIFY_START(1);
      JUSTIFY_LAST(false);
      write_sstring_case(stream_,*string_); # String ausgeben
      JUSTIFY_SPACE;
      JUSTIFY_LAST(true);
      prin_object(stream_,*(string_ STACKop 1)); # other ausgeben
      JUSTIFY_END_ENG;
      INDENT_END;
      write_ascii_char(stream_,'>');
      skipSTACK(2);
    }

# UP: Gibt ein SUBR auf einen Stream aus.
# pr_subr(&stream,obj);
# > obj: SUBR
# > stream: Stream
# < stream: Stream
# can trigger GC
  local void pr_subr(stream_,obj)
    var const object* stream_;
    var object obj;
    {
      # #<SYSTEM-FUNCTION name> bzw. #<ADD-ON-SYSTEM-FUNCTION name>
      # bzw. #.(SYSTEM::%FIND-SUBR 'name)
      if (test_value(S(print_readably))) {
        if (!(test_value(S(read_eval)) || stream_get_read_eval(*stream_)))
          fehler_print_readably(obj);
        pushSTACK(obj); # obj retten
        var object* obj_ = &STACK_0; # und merken, wo es sitzt
        write_ascii_char(stream_,'#'); write_ascii_char(stream_,'.');
        KLAMMER_AUF; # '('
        INDENT_START(3); # um 3 Zeichen einrücken, wegen '#.('
        JUSTIFY_START(1);
        JUSTIFY_LAST(false);
        pr_symbol(stream_,S(find_subr)); # SYSTEM::%FIND-SUBR
        JUSTIFY_SPACE;
        JUSTIFY_LAST(true);
        write_ascii_char(stream_,'\'');
        pr_symbol(stream_,TheSubr(*obj_)->name); # Name ausgeben
        JUSTIFY_END_ENG;
        INDENT_END;
        KLAMMER_ZU;
        skipSTACK(1);
      } else {
        pr_other_obj(stream_,TheSubr(obj)->name,
                     ((as_oint(subr_tab_ptr_as_object(&subr_tab)) <= as_oint(obj))
                      && (as_oint(obj) < as_oint(subr_tab_ptr_as_object(&subr_tab+1)))
                     ) ? O(printstring_subr) : O(printstring_addon_subr)
                    );
      }
    }

# UP: Gibt ein FSUBR auf einen Stream aus.
# pr_fsubr(&stream,obj);
# > obj: FSUBR
# > stream: Stream
# < stream: Stream
# can trigger GC
  local void pr_fsubr(stream_,obj)
    var const object* stream_;
    var object obj;
    {
      # #<SPECIAL-OPERATOR name>
      if (test_value(S(print_readably)))
        fehler_print_readably(obj);
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
    {
      if (simple_bit_vector_p(Atype_8Bit,TheClosure(obj)->clos_codevec)) {
        # compilierte Closure
        pr_cclosure(stream_,obj);
      } else {
        # interpretierte Closure ausgeben: #<CLOSURE ...>
        # Falls *PRINT-CLOSURE* /= NIL, alles, sonst den Namen und
        # (falls noch vorhanden) Lambdaliste und Formen, ausgeben:
        if (test_value(S(print_readably)))
          fehler_print_readably(obj);
        LEVEL_CHECK;
        {
          pushSTACK(obj); # Closure retten
          var object* obj_ = &STACK_0; # und merken, wo sie sitzt
          write_ascii_char(stream_,'#'); write_ascii_char(stream_,'<');
          INDENT_START(2); # um 2 Zeichen einrücken, wegen '#<'
          JUSTIFY_START(1);
          JUSTIFY_LAST(false);
          write_sstring_case(stream_,O(printstring_closure));
          if (test_value(S(print_closure))) { # *PRINT-CLOSURE* abfragen
            # *PRINT-CLOSURE* /= NIL -> #<CLOSURE komponente1 ...> ausgeben:
            pr_record_ab(stream_,obj_,0,0); # alle weiteren Komponenten ausgeben
          } else {
            # *PRINT-CLOSURE* = NIL -> #<CLOSURE name . form> ausgeben:
            JUSTIFY_SPACE;
            prin_object(stream_,TheIclosure(*obj_)->clos_name); # Name ausgeben
            # Formenliste elementweise ausgeben:
            pr_record_rest(stream_,TheIclosure(*obj_)->clos_form,1);
          }
          JUSTIFY_END_ENG;
          INDENT_END;
          write_ascii_char(stream_,'>');
          skipSTACK(1);
        }
        LEVEL_END;
      }
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
    {
      # *PRINT-CLOSURE* abfragen:
      if (test_value(S(print_closure)) || test_value(S(print_readably))) {
        # *PRINT-CLOSURE /= NIL -> in wiedereinlesbarer Form #Y(...) ausgeben
        pr_cclosure_lang(stream_,obj);
      } else {
        # *PRINT-CLOSURE* = NIL ->
        # nur #<GENERIC-FUNCTION name> bzw. #<COMPILED-CLOSURE name> ausgeben:
        pr_other_obj(stream_,TheClosure(obj)->clos_name,
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
    {
      LEVEL_CHECK;
      {
        pushSTACK(obj); # Closure retten
        var object* obj_ = &STACK_0; # und merken, wo sie sitzt
        write_ascii_char(stream_,'#'); write_ascii_char(stream_,'Y');
        KLAMMER_AUF;
        INDENT_START(3); # um 3 Zeichen einrücken, wegen '#Y('
        JUSTIFY_START(1);
        JUSTIFY_LAST(false);
        prin_object(stream_,TheClosure(*obj_)->clos_name); # Name ausgeben
        JUSTIFY_SPACE;
        # Codevektor byteweise ausgeben, dabei Zirkularität behandeln:
        pr_circle(stream_,TheClosure(*obj_)->clos_codevec,&pr_cclosure_codevector);
        pr_record_ab(stream_,obj_,2,2); # restliche Komponenten ausgeben
        JUSTIFY_END_ENG;
        INDENT_END;
        KLAMMER_ZU;
        skipSTACK(1);
      }
      LEVEL_END;
    }

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
    {
      LEVEL_CHECK;
      {
        pushSTACK(codevec); # Codevektor retten
        var object* codevec_ = &STACK_0; # und merken, wo er sitzt
        var uintL len = Sbvector_length(codevec); # Länge in Bytes
        #if BIG_ENDIAN_P
        var uintL header_end_index =
          (TheSbvector(codevec)->data[CCV_FLAGS] & bit(7) ? CCV_START_KEY : CCV_START_NONKEY);
        #endif
        # Präfix ausgeben:
        INDENTPREP_START;
        write_ascii_char(stream_,'#');
        pr_uint(stream_,len); # Länge dezimal ausgeben
        write_ascii_char(stream_,'Y');
        {
          var uintL indent = INDENTPREP_END;
        # Hauptteil ausgeben:
          INDENT_START(indent); # einrücken
        }
        KLAMMER_AUF;
        INDENT_START(1); # um 1 Zeichen einrücken, wegen '('
        JUSTIFY_START(1);
        {
          var uintL length_limit = get_print_length(); # *PRINT-LENGTH*-Begrenzung
          var uintL length = 0; # Index = bisherige Länge := 0
          for ( ; len > 0; len--) {
            # (außer vorm ersten Element) Space ausgeben:
            if (!(length==0))
              JUSTIFY_SPACE;
            # auf Erreichen von *PRINT-LENGTH* prüfen:
            if (length >= length_limit) {
              JUSTIFY_LAST(true);
              # Rest durch '...' abkürzen:
              write_ascii_char(stream_,'.');
              write_ascii_char(stream_,'.');
              write_ascii_char(stream_,'.');
              break;
            }
            JUSTIFY_LAST(len==1 || length+1 >= length_limit);
            codevec = *codevec_;
            var uintL index = length;
            #if BIG_ENDIAN_P
            # Byte-Index berechnen, dabei Big-Endian -> Little-Endian Konversion machen:
            if (index < header_end_index) {
              switch (index) {
                case CCV_SPDEPTH_1:          case CCV_SPDEPTH_1+1:
                case CCV_SPDEPTH_JMPBUFSIZE: case CCV_SPDEPTH_JMPBUFSIZE+1:
                case CCV_NUMREQ:             case CCV_NUMREQ+1:
                case CCV_NUMOPT:             case CCV_NUMOPT+1:
                case CCV_NUMKEY:             case CCV_NUMKEY+1:
                case CCV_KEYCONSTS:          case CCV_KEYCONSTS+1:
                  index = index^1;
                  break;
                default:
                  break;
              }
            }
            #endif
            # Byte ausgeben:
            pr_hex2(stream_,TheSbvector(codevec)->data[index]);
            length++; # Index erhöhen
          }
        }
        JUSTIFY_END_ENG;
        INDENT_END;
        KLAMMER_ZU;
        INDENT_END;
        skipSTACK(1);
      }
      LEVEL_END;
    }

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
    {
      if (test_value(S(print_readably)))
        fehler_print_readably(obj);
      pushSTACK(obj); # Stream retten
      var object* obj_ = &STACK_0; # und merken, wo er sitzt
      write_ascii_char(stream_,'#'); write_ascii_char(stream_,'<');
      INDENT_START(2); # um 2 Zeichen einrücken, wegen '#<'
      JUSTIFY_START(1);
      JUSTIFY_LAST(false);
      # falls Stream geschlossen, "CLOSED " ausgeben:
      if ((TheStream(*obj_)->strmflags & strmflags_open_B) == 0)
        write_sstring_case(stream_,O(printstring_closed));
      # if a channel or socket stream, print "BUFFERED " or "UNBUFFERED ":
      var uintL type = TheStream(*obj_)->strmtype;
      switch (type) {
        case strmtype_file:
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
      {
        var const object* stringtable = &O(printstring_strmtype_synonym);
        write_sstring_case(stream_,stringtable[type]); # String aus Tabelle holen
      }
      # "-STREAM" ausgeben:
      write_sstring_case(stream_,O(printstring_stream));
      # Streamspezifische Zusatzinformation:
      switch (type) {
        case strmtype_synonym:
          # Synonym-Stream
          JUSTIFY_SPACE;
          JUSTIFY_LAST(true);
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
          JUSTIFY_LAST(true);
          prin_object(stream_,TheStream(*obj_)->strm_buff_in_fun); # Funktion ausgeben
          break;
        case strmtype_buff_out:
          # Buffered-Output-Stream
          JUSTIFY_SPACE;
          JUSTIFY_LAST(true);
          prin_object(stream_,TheStream(*obj_)->strm_buff_out_fun); # Funktion ausgeben
          break;
        #ifdef GENERIC_STREAMS
        case strmtype_generic:
          # Generic Streams
          JUSTIFY_SPACE;
          JUSTIFY_LAST(true);
          prin_object(stream_,TheStream(*obj_)->strm_controller_object); # Controller ausgeben
          break;
        #endif
        case strmtype_file:
          # File-Stream
          JUSTIFY_SPACE;
          JUSTIFY_LAST(nullp(TheStream(*obj_)->strm_file_name) && !eq(TheStream(*obj_)->strm_eltype,S(character)));
          prin_object(stream_,TheStream(*obj_)->strm_eltype); # Stream-Element-Type
          if (!nullp(TheStream(*obj_)->strm_file_name)) {
            JUSTIFY_SPACE;
            JUSTIFY_LAST(!eq(TheStream(*obj_)->strm_eltype,S(character)));
            prin_object(stream_,TheStream(*obj_)->strm_file_name); # Filename ausgeben
          }
          if (eq(TheStream(*obj_)->strm_eltype,S(character))) {
            JUSTIFY_SPACE;
            JUSTIFY_LAST(true);
            # Zeilennummer ausgeben, in der sich der Stream gerade befindet:
            write_ascii_char(stream_,'@');
            pr_number(stream_,stream_line_number(*obj_));
          }
          break;
        #ifdef PIPES
        case strmtype_pipe_in:
        case strmtype_pipe_out:
          # Pipe-In/Out-Stream
          JUSTIFY_SPACE;
          JUSTIFY_LAST(false);
          prin_object(stream_,TheStream(*obj_)->strm_eltype); # Stream-Element-Type
          JUSTIFY_SPACE;
          JUSTIFY_LAST(true);
          pr_uint(stream_,I_to_UL(TheStream(*obj_)->strm_pipe_pid)); # Prozess-Id ausgeben
          break;
        #endif
        #ifdef X11SOCKETS
        case strmtype_x11socket:
          # X11-Socket-Stream
          JUSTIFY_SPACE;
          JUSTIFY_LAST(true);
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
          JUSTIFY_LAST(false);
          prin_object(stream_,TheStream(*obj_)->strm_eltype); # Stream-Element-Type
          JUSTIFY_SPACE;
          JUSTIFY_LAST(true);
          {
            var object host = TheStream(*obj_)->strm_socket_host;
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
      JUSTIFY_END_ENG;
      INDENT_END;
      write_ascii_char(stream_,'>');
      skipSTACK(1);
    }


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
    {
      pr_enter(stream_,obj,&prin_object);
    }

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
    {
      pushSTACK(obj); # Objekt retten
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
    {
      var object stream = STACK_0; # Output-Stream-Argument
      if (eq(stream,unbound) || nullp(stream)) {
        # #<UNBOUND> oder NIL -> Wert von *STANDARD-OUTPUT*
        STACK_0 = var_stream(S(standard_output),strmflags_wr_ch_B);
      } elif (eq(stream,T)) {
        # T -> Wert von *TERMINAL-IO*
        STACK_0 = var_stream(S(terminal_io),strmflags_wr_ch_B);
      } else {
        # sollte ein Stream sein
        if (!streamp(stream))
          fehler_stream(stream);
      }
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
    {
      var object* argptr = args_end_pointer STACKop (1+print_vars_anz+1); # Pointer über die Keyword-Argumente
      var object obj = NEXT(argptr); # erstes Argument = Objekt
      # die angegebenen Variablen binden:
      var uintC bindcount = 0; # Anzahl der Bindungen
      {
        var object sym = first_print_var; # durchläuft die Symbole
        var uintC count;
        dotimesC(count,print_vars_anz, {
          var object arg = NEXT(argptr); # nächstes Keyword-Argument
          if (!eq(arg,unbound)) { # angegeben?
            dynamic_bind(sym,arg); bindcount++; # ja -> Variable daran binden
          }
          sym = objectplus(sym,(soint)sizeof(*TheSymbol(sym))<<(oint_addr_shift-addr_shift)); # zum nächsten Symbol
        });
      }
      {
        var object* stream_ = &NEXT(argptr); # nächstes Argument ist der Stream
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
# CLTL p. 382
  {
    # stack layout: object, Print-Variablen-Argumente, Stream-Argument.
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
    {
      var object obj = STACK_1;
      var object* stream_ = &STACK_0;
      dynamic_bind(S(print_escape),T); # *PRINT-ESCAPE* an T binden
      prin1(stream_,obj); # object ausgeben
      dynamic_unbind();
    }

LISPFUN(prin1,1,1,norest,nokey,0,NIL)
# (PRIN1 object [stream]), CLTL p. 383
  {
    test_ostream(); # Output-Stream überprüfen
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
# (PRINT object [stream]), CLTL p. 383
  {
    test_ostream(); # Output-Stream überprüfen
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
# (PPRINT object [stream]), CLTL p. 383
  {
    test_ostream(); # Output-Stream überprüfen
    terpri(&STACK_0); # neue Zeile
    var object obj = STACK_1;
    var object* stream_ = &STACK_0;
    dynamic_bind(S(print_pretty),T); # *PRINT-PRETTY* an T binden
    dynamic_bind(S(print_escape),T); # *PRINT-ESCAPE* an T binden
    prin1(stream_,obj); # object ausgeben
    dynamic_unbind();
    dynamic_unbind();
    skipSTACK(2);
    value1 = NIL; mv_count=0; # keine Werte
  }

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
    {
      var object obj = STACK_1;
      var object* stream_ = &STACK_0;
      dynamic_bind(S(print_escape),NIL); # *PRINT-ESCAPE* an NIL binden
      dynamic_bind(S(print_readably),NIL); # *PRINT-READABLY* an NIL binden
      prin1(stream_,obj); # object ausgeben
      dynamic_unbind();
      dynamic_unbind();
    }

LISPFUN(princ,1,1,norest,nokey,0,NIL)
# (PRINC object [stream]), CLTL p. 383
  {
    test_ostream(); # Output-Stream überprüfen
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
#                         [:closure] [:readably] [:right-margin]),
# CLTL p. 383
  {
    pushSTACK(make_string_output_stream()); # String-Output-Stream
    write_up(); # WRITE durchführen
    value1 = get_output_stream_string(&STACK_0); mv_count=1; # Ergebnis-String als Wert
    skipSTACK(1+print_vars_anz+1);
  }

# (defun prin1-to-string (object)
#   (with-output-to-string (stream) (prin1 object stream))
# )
LISPFUNN(prin1_to_string,1)
# (PRIN1-TO-STRING object), CLTL p. 383
  {
    pushSTACK(make_string_output_stream()); # String-Output-Stream
    prin1_up(); # PRIN1 durchführen
    value1 = get_output_stream_string(&STACK_0); mv_count=1; # Ergebnis-String als Wert
    skipSTACK(2);
  }

# (defun princ-to-string (object)
#   (with-output-to-string (stream) (princ object stream))
# )
LISPFUNN(princ_to_string,1)
# (PRINC-TO-STRING object), CLTL p. 383
  {
    pushSTACK(make_string_output_stream()); # String-Output-Stream
    princ_up(); # PRINC durchführen
    value1 = get_output_stream_string(&STACK_0); mv_count=1; # Ergebnis-String als Wert
    skipSTACK(2);
  }

LISPFUN(write_char,1,1,norest,nokey,0,NIL)
# (WRITE-CHAR character [stream]), CLTL p. 384
  {
    test_ostream(); # Output-Stream überprüfen
    var object ch = STACK_1; # character-Argument
    if (!charp(ch))
      fehler_char(ch);
    write_char(&STACK_0,ch);
    value1 = ch; mv_count=1; # ch (nicht GC-gefährdet) als Wert
    skipSTACK(2);
  }

# UP für WRITE-STRING und WRITE-LINE:
# Überprüft die Argumente und gibt einen String-Teil auf einen Stream aus.
# > subr_self: Aufrufer (ein SUBR)
# > stack layout: String-Argument, Stream-Argument, :START-Argument, :END-Argument.
# < stack layout: Stream, String.
# can trigger GC
  local void write_string_up (void);
  local void write_string_up()
    {
      pushSTACK(STACK_2); # Stream ans STACK-Ende
      test_ostream(); # überprüfen
      STACK_(2+1) = STACK_(3+1);
      STACK_(3+1) = STACK_0;
      skipSTACK(1);
      # stack layout: stream, string, :START-Argument, :END-Argument.
      # Grenzen überprüfen:
      var stringarg arg;
      var object string = test_string_limits_ro(&arg);
      pushSTACK(string);
      # stack layout: stream, string.
      write_sstring_ab(&STACK_1,arg.string,arg.offset+arg.index,arg.len);
    }

LISPFUN(write_string,1,1,norest,key,2, (kw(start),kw(end)) )
# (WRITE-STRING string [stream] [:start] [:end]), CLTL p. 384
  {
    write_string_up(); # überprüfen und ausgeben
    value1 = popSTACK(); mv_count=1; skipSTACK(1); # string als Wert
  }

LISPFUN(write_line,1,1,norest,key,2, (kw(start),kw(end)) )
# (WRITE-LINE string [stream] [:start] [:end]), CLTL p. 384
  {
    write_string_up(); # überprüfen und ausgeben
    terpri(&STACK_1); # neue Zeile
    value1 = popSTACK(); mv_count=1; skipSTACK(1); # string als Wert
  }

LISPFUN(terpri,0,1,norest,nokey,0,NIL)
# (TERPRI [stream]), CLTL p. 384
  {
    test_ostream(); # Output-Stream überprüfen
    terpri(&STACK_0); # neue Zeile
    value1 = NIL; mv_count=1; skipSTACK(1); # NIL als Wert
  }

LISPFUN(fresh_line,0,1,norest,nokey,0,NIL)
# (FRESH-LINE [stream]), CLTL p. 384
  {
    test_ostream(); # Output-Stream überprüfen
    if (eq(get_line_position(STACK_0),Fixnum_0)) { # Line-Position = 0 ?
      value1 = NIL; mv_count=1; # ja -> NIL als Wert
    } else {
      terpri(&STACK_0); # nein -> neue Zeile
      value1 = T; mv_count=1; # und T als Wert
    }
    skipSTACK(1);
  }

LISPFUN(finish_output,0,1,norest,nokey,0,NIL)
# (FINISH-OUTPUT [stream]), CLTL p. 384
  {
    test_ostream(); # Output-Stream überprüfen
    finish_output(popSTACK()); # Output ans Ziel bringen
    value1 = NIL; mv_count=1; # NIL als Wert
  }

LISPFUN(force_output,0,1,norest,nokey,0,NIL)
# (FORCE-OUTPUT [stream]), CLTL p. 384
  {
    test_ostream(); # Output-Stream überprüfen
    force_output(popSTACK()); # Output ans Ziel bringen
    value1 = NIL; mv_count=1; # NIL als Wert
  }

LISPFUN(clear_output,0,1,norest,nokey,0,NIL)
# (CLEAR-OUTPUT [stream]), CLTL p. 384
  {
    test_ostream(); # Output-Stream überprüfen
    clear_output(popSTACK()); # Output löschen
    value1 = NIL; mv_count=1; # NIL als Wert
  }

LISPFUN(write_unreadable,3,0,norest,key,2, (kw(type),kw(identity)) )
# (SYSTEM::WRITE-UNREADABLE function object stream [:type] [:identity]),
# vgl. CLtL2 S. 580
  {
    var bool flag_fun = false;
    var bool flag_type = false;
    var bool flag_id = false;
    {
      var object arg = popSTACK(); # :identity - Argument
      if (!(eq(arg,unbound) || nullp(arg)))
        flag_id = true;
    }
    {
      var object arg = popSTACK(); # :type - Argument
      if (!(eq(arg,unbound) || nullp(arg)))
        flag_type = true;
    }
    if (!nullp(STACK_2))
      flag_fun = true;
    test_ostream(); # Output-Stream überprüfen
    if (test_value(S(print_readably)))
      fehler_print_readably(STACK_1);
    var object* stream_ = &STACK_0;
    write_ascii_char(stream_,'#'); write_ascii_char(stream_,'<');
    INDENT_START(2); # um 2 Zeichen einrücken, wegen '#<'
    JUSTIFY_START(1);
    if (flag_type) {
      JUSTIFY_LAST(!flag_fun && !flag_id);
      # (TYPE-OF object) ausgeben:
      pushSTACK(*(stream_ STACKop 1)); funcall(L(type_of),1);
      prin1(stream_,value1);
      if (flag_fun || flag_id)
        JUSTIFY_SPACE;
    }
    if (flag_fun) {
      JUSTIFY_LAST(!flag_id);
      funcall(*(stream_ STACKop 2),0); # (FUNCALL function)
    }
    if (flag_id) {
      if (flag_fun)
        JUSTIFY_SPACE;
      JUSTIFY_LAST(true);
      pr_hex6(stream_,*(stream_ STACKop 1));
    }
    JUSTIFY_END_ENG;
    INDENT_END;
    write_ascii_char(stream_,'>');
    skipSTACK(3);
    value1 = NIL; mv_count=1;
  }

LISPFUN(line_position,0,1,norest,nokey,0,NIL)
# (SYS::LINE-POSITION [stream]), Hilfsfunktion für FORMAT ~T,
# liefert die Position eines (Output-)Streams in der momentanen Zeile, oder NIL.
  {
    test_ostream(); # Output-Stream überprüfen
    value1 = get_line_position(popSTACK()); mv_count=1; # Line-Position als Wert
  }

