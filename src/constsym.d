# Liste aller dem C-Programm bekannten Symbole ("Programmkonstanten")
# Bruno Haible 1990-1999

# Der Macro LISPSYM deklariert ein LISP-Symbol.
# LISPSYM(name,printname,package)
# > name: C-Name des Symbols.
# > printname: Printname des Symbols (ein C-String).
# > package: Home-Package des Symbols, entweder lisp oder system oder keyword.
# >          Aus der Package lisp wird es automatisch exportiert.

# Expander für die Deklaration der Symbol-Tabelle:
  #define LISPSYM_A(name,printname,package)  \
    symbol_ S_##name;

# Expander für die Initialisierung der Symbol-Tabelle:
  #ifdef TYPECODES
    #define LISPSYM_B(name,printname,package)  \
      { {S(name)}, unbound, unbound, NIL, NIL, NIL, },
  #else
    #define LISPSYM_B(name,printname,package)  \
      { S(name), xrecord_tfl(Rectype_Symbol,0,5,0), \
        unbound, unbound, NIL, NIL, NIL,            \
      },
  #endif
  #define LISPSYM_C(name,printname,package)  printname,
  #define LISPSYM_D(name,printname,package)  (uintB)enum_##package##_index,

# Welcher Expander benutzt wird, muss vom Hauptfile aus eingestellt werden.


LISPSYM(nil,"NIL",lisp)
LISPSYM(t,"T",lisp)

# FSUBRs in CONTROL:
LISPSYM(eval_when,"EVAL-WHEN",lisp)
LISPSYM(quote,"QUOTE",lisp)
LISPSYM(function,"FUNCTION",lisp)
LISPSYM(setq,"SETQ",lisp)
LISPSYM(psetq,"PSETQ",lisp)
LISPSYM(progn,"PROGN",lisp)
LISPSYM(prog1,"PROG1",lisp)
LISPSYM(prog2,"PROG2",lisp)
LISPSYM(let,"LET",lisp)
LISPSYM(letstern,"LET*",lisp)
LISPSYM(locally,"LOCALLY",lisp)
LISPSYM(compiler_let,"COMPILER-LET",lisp)
LISPSYM(progv,"PROGV",lisp)
LISPSYM(flet,"FLET",lisp)
LISPSYM(labels,"LABELS",lisp)
LISPSYM(macrolet,"MACROLET",lisp)
LISPSYM(symbol_macrolet,"SYMBOL-MACROLET",lisp)
LISPSYM(if,"IF",lisp)
LISPSYM(when,"WHEN",lisp)
LISPSYM(unless,"UNLESS",lisp)
LISPSYM(cond,"COND",lisp)
LISPSYM(case,"CASE",lisp)
LISPSYM(block,"BLOCK",lisp)
LISPSYM(return_from,"RETURN-FROM",lisp)
LISPSYM(tagbody,"TAGBODY",lisp)
LISPSYM(go,"GO",lisp)
LISPSYM(multiple_value_list,"MULTIPLE-VALUE-LIST",lisp)
LISPSYM(multiple_value_call,"MULTIPLE-VALUE-CALL",lisp)
LISPSYM(multiple_value_prog1,"MULTIPLE-VALUE-PROG1",lisp)
LISPSYM(multiple_value_bind,"MULTIPLE-VALUE-BIND",lisp)
LISPSYM(multiple_value_setq,"MULTIPLE-VALUE-SETQ",lisp)
LISPSYM(catch,"CATCH",lisp)
LISPSYM(unwind_protect,"UNWIND-PROTECT",lisp)
LISPSYM(throw,"THROW",lisp)
LISPSYM(declare,"DECLARE",lisp)
LISPSYM(the,"THE",lisp)
LISPSYM(load_time_value,"LOAD-TIME-VALUE",lisp)
LISPSYM(and,"AND",lisp)
LISPSYM(or,"OR",lisp)

# SUBRs:
# ---------- SPVW ----------
# keine SUBRs
# ---------- EVAL ----------
LISPSYM(funtabref,"%FUNTABREF",system)
LISPSYM(subr_info,"SUBR-INFO",system)
# ---------- ARRAY ----------
LISPSYM(copy_simple_vector,"%COPY-SIMPLE-VECTOR",system)
LISPSYM(vector,"VECTOR",lisp)
LISPSYM(aref,"AREF",lisp)
LISPSYM(store,"STORE",system)
LISPSYM(svref,"SVREF",lisp)
LISPSYM(svstore,"SVSTORE",system)
LISPSYM(psvstore,"%SVSTORE",system)
LISPSYM(row_major_aref,"ROW-MAJOR-AREF",lisp)
LISPSYM(row_major_store,"ROW-MAJOR-STORE",system)
LISPSYM(array_element_type,"ARRAY-ELEMENT-TYPE",lisp)
LISPSYM(array_rank,"ARRAY-RANK",lisp)
LISPSYM(array_dimension,"ARRAY-DIMENSION",lisp)
LISPSYM(array_dimensions,"ARRAY-DIMENSIONS",lisp)
LISPSYM(array_total_size,"ARRAY-TOTAL-SIZE",lisp)
LISPSYM(array_in_bounds_p,"ARRAY-IN-BOUNDS-P",lisp)
LISPSYM(array_row_major_index,"ARRAY-ROW-MAJOR-INDEX",lisp)
LISPSYM(adjustable_array_p,"ADJUSTABLE-ARRAY-P",lisp)
LISPSYM(array_displacement,"ARRAY-DISPLACEMENT",lisp)
LISPSYM(bit,"BIT",lisp)
LISPSYM(sbit,"SBIT",lisp)
LISPSYM(bit_and,"BIT-AND",lisp)
LISPSYM(bit_ior,"BIT-IOR",lisp)
LISPSYM(bit_xor,"BIT-XOR",lisp)
LISPSYM(bit_eqv,"BIT-EQV",lisp)
LISPSYM(bit_nand,"BIT-NAND",lisp)
LISPSYM(bit_nor,"BIT-NOR",lisp)
LISPSYM(bit_andc1,"BIT-ANDC1",lisp)
LISPSYM(bit_andc2,"BIT-ANDC2",lisp)
LISPSYM(bit_orc1,"BIT-ORC1",lisp)
LISPSYM(bit_orc2,"BIT-ORC2",lisp)
LISPSYM(bit_not,"BIT-NOT",lisp)
LISPSYM(array_has_fill_pointer_p,"ARRAY-HAS-FILL-POINTER-P",lisp)
LISPSYM(fill_pointer,"FILL-POINTER",lisp)
LISPSYM(set_fill_pointer,"SET-FILL-POINTER",system)
LISPSYM(vector_push,"VECTOR-PUSH",lisp)
LISPSYM(vector_pop,"VECTOR-POP",lisp)
LISPSYM(vector_push_extend,"VECTOR-PUSH-EXTEND",lisp)
LISPSYM(make_array,"MAKE-ARRAY",lisp)
LISPSYM(adjust_array,"ADJUST-ARRAY",lisp)
LISPSYM(vector_init,"VECTOR-INIT",system)
LISPSYM(vector_upd,"VECTOR-UPD",system)
LISPSYM(vector_endtest,"VECTOR-ENDTEST",system)
LISPSYM(vector_fe_init,"VECTOR-FE-INIT",system)
LISPSYM(vector_fe_upd,"VECTOR-FE-UPD",system)
LISPSYM(vector_fe_endtest,"VECTOR-FE-ENDTEST",system)
LISPSYM(vector_length,"VECTOR-LENGTH",system)
LISPSYM(vector_init_start,"VECTOR-INIT-START",system)
LISPSYM(vector_fe_init_end,"VECTOR-FE-INIT-END",system)
LISPSYM(make_bit_vector,"MAKE-BIT-VECTOR",system)
# ---------- CHARSTRG ----------
LISPSYM(standard_char_p,"STANDARD-CHAR-P",lisp)
LISPSYM(graphic_char_p,"GRAPHIC-CHAR-P",lisp)
LISPSYM(string_char_p,"STRING-CHAR-P",lisp)
#if (base_char_code_limit < char_code_limit)
LISPSYM(base_char_p,"BASE-CHAR-P",system)
#endif
LISPSYM(alpha_char_p,"ALPHA-CHAR-P",lisp)
LISPSYM(upper_case_p,"UPPER-CASE-P",lisp)
LISPSYM(lower_case_p,"LOWER-CASE-P",lisp)
LISPSYM(both_case_p,"BOTH-CASE-P",lisp)
LISPSYM(digit_char_p,"DIGIT-CHAR-P",lisp)
LISPSYM(alphanumericp,"ALPHANUMERICP",lisp)
LISPSYM(char_gleich,"CHAR=",lisp)
LISPSYM(char_ungleich,"CHAR/=",lisp)
LISPSYM(char_kleiner,"CHAR<",lisp)
LISPSYM(char_groesser,"CHAR>",lisp)
LISPSYM(char_klgleich,"CHAR<=",lisp)
LISPSYM(char_grgleich,"CHAR>=",lisp)
LISPSYM(char_equal,"CHAR-EQUAL",lisp)
LISPSYM(char_not_equal,"CHAR-NOT-EQUAL",lisp)
LISPSYM(char_lessp,"CHAR-LESSP",lisp)
LISPSYM(char_greaterp,"CHAR-GREATERP",lisp)
LISPSYM(char_not_greaterp,"CHAR-NOT-GREATERP",lisp)
LISPSYM(char_not_lessp,"CHAR-NOT-LESSP",lisp)
LISPSYM(char_code,"CHAR-CODE",lisp)
LISPSYM(code_char,"CODE-CHAR",lisp)
LISPSYM(character,"CHARACTER",lisp)
LISPSYM(char_upcase,"CHAR-UPCASE",lisp)
LISPSYM(char_downcase,"CHAR-DOWNCASE",lisp)
LISPSYM(digit_char,"DIGIT-CHAR",lisp)
LISPSYM(char_int,"CHAR-INT",lisp)
LISPSYM(int_char,"INT-CHAR",lisp)
LISPSYM(char_name,"CHAR-NAME",lisp)
LISPSYM(char,"CHAR",lisp)
LISPSYM(schar,"SCHAR",lisp)
LISPSYM(store_char,"STORE-CHAR",system)
LISPSYM(store_schar,"STORE-SCHAR",system)
LISPSYM(string_gleich,"STRING=",lisp)
LISPSYM(string_ungleich,"STRING/=",lisp)
LISPSYM(string_kleiner,"STRING<",lisp)
LISPSYM(string_groesser,"STRING>",lisp)
LISPSYM(string_klgleich,"STRING<=",lisp)
LISPSYM(string_grgleich,"STRING>=",lisp)
LISPSYM(string_equal,"STRING-EQUAL",lisp)
LISPSYM(string_not_equal,"STRING-NOT-EQUAL",lisp)
LISPSYM(string_lessp,"STRING-LESSP",lisp)
LISPSYM(string_greaterp,"STRING-GREATERP",lisp)
LISPSYM(string_not_greaterp,"STRING-NOT-GREATERP",lisp)
LISPSYM(string_not_lessp,"STRING-NOT-LESSP",lisp)
LISPSYM(search_string_gleich,"SEARCH-STRING=",system)
LISPSYM(search_string_equal,"SEARCH-STRING-EQUAL",system)
LISPSYM(make_string,"MAKE-STRING",lisp)
LISPSYM(string_both_trim,"STRING-BOTH-TRIM",system)
LISPSYM(nstring_upcase,"NSTRING-UPCASE",lisp)
LISPSYM(string_upcase,"STRING-UPCASE",lisp)
LISPSYM(nstring_downcase,"NSTRING-DOWNCASE",lisp)
LISPSYM(string_downcase,"STRING-DOWNCASE",lisp)
LISPSYM(nstring_capitalize,"NSTRING-CAPITALIZE",lisp)
LISPSYM(string_capitalize,"STRING-CAPITALIZE",lisp)
LISPSYM(string,"STRING",lisp)
LISPSYM(name_char,"NAME-CHAR",lisp)
LISPSYM(substring,"SUBSTRING",lisp)
LISPSYM(string_concat,"STRING-CONCAT",lisp)
# ---------- CONTROL ----------
LISPSYM(exit,"%EXIT",system)
LISPSYM(psymbol_value,"%SYMBOL-VALUE",system)
LISPSYM(symbol_value,"SYMBOL-VALUE",lisp)
LISPSYM(symbol_function,"SYMBOL-FUNCTION",lisp)
LISPSYM(fdefinition,"FDEFINITION",lisp)
LISPSYM(boundp,"BOUNDP",lisp)
LISPSYM(fboundp,"FBOUNDP",lisp)
LISPSYM(special_operator_p,"SPECIAL-OPERATOR-P",lisp)
LISPSYM(set,"SET-SYMBOL-VALUE",system)
LISPSYM(makunbound,"MAKUNBOUND",lisp)
LISPSYM(fmakunbound,"FMAKUNBOUND",lisp)
LISPSYM(apply,"APPLY",lisp)
LISPSYM(pfuncall,"%FUNCALL",system)
LISPSYM(funcall,"FUNCALL",lisp)
LISPSYM(mapcar,"MAPCAR",lisp)
LISPSYM(maplist,"MAPLIST",lisp)
LISPSYM(mapc,"MAPC",lisp)
LISPSYM(mapl,"MAPL",lisp)
LISPSYM(mapcan,"MAPCAN",lisp)
LISPSYM(mapcon,"MAPCON",lisp)
LISPSYM(values,"VALUES",lisp)
LISPSYM(values_list,"VALUES-LIST",lisp)
LISPSYM(driver,"DRIVER",system)
LISPSYM(unwind_to_driver,"UNWIND-TO-DRIVER",system)
LISPSYM(macro_function,"MACRO-FUNCTION",lisp)
LISPSYM(old_macro_function,"OLD-MACRO-FUNCTION",system)
LISPSYM(macroexpand,"MACROEXPAND",lisp)
LISPSYM(macroexpand_1,"MACROEXPAND-1",lisp)
LISPSYM(proclaim,"PROCLAIM",lisp)
LISPSYM(eval,"EVAL",lisp)
LISPSYM(evalhook,"EVALHOOK",lisp)
LISPSYM(applyhook,"APPLYHOOK",lisp)
LISPSYM(constantp,"CONSTANTP",lisp)
LISPSYM(function_name_p,"FUNCTION-NAME-P",system)
LISPSYM(parse_body,"PARSE-BODY",system)
LISPSYM(keyword_test,"KEYWORD-TEST",system)
# ---------- DEBUG ----------
LISPSYM(read_form,"READ-FORM",system)
LISPSYM(read_eval_print,"READ-EVAL-PRINT",system)
LISPSYM(load,"LOAD",lisp)
LISPSYM(frame_up_1,"FRAME-UP-1",system)
LISPSYM(frame_up,"FRAME-UP",system)
LISPSYM(frame_down_1,"FRAME-DOWN-1",system)
LISPSYM(frame_down,"FRAME-DOWN",system)
LISPSYM(the_frame,"THE-FRAME",system)
LISPSYM(same_env_as,"SAME-ENV-AS",system)
LISPSYM(eval_at,"EVAL-AT",system)
LISPSYM(eval_frame_p,"EVAL-FRAME-P",system)
LISPSYM(driver_frame_p,"DRIVER-FRAME-P",system)
LISPSYM(trap_eval_frame,"TRAP-EVAL-FRAME",system)
LISPSYM(redo_eval_frame,"REDO-EVAL-FRAME",system)
LISPSYM(return_from_eval_frame,"RETURN-FROM-EVAL-FRAME",system)
LISPSYM(describe_frame,"DESCRIBE-FRAME",system)
LISPSYM(show_stack,"SHOW-STACK",lisp)
LISPSYM(debug,"DEBUG",system)
LISPSYM(proom,"%ROOM",system)
LISPSYM(gc,"GC",lisp)
# ---------- ENCODING ----------
LISPSYM(make_encoding,"MAKE-ENCODING",lisp)
LISPSYM(encodingp,"ENCODINGP",system)
LISPSYM(charset_typep,"CHARSET-TYPEP",system)
LISPSYM(charset_range,"CHARSET-RANGE",system)
LISPSYM(default_file_encoding,"DEFAULT-FILE-ENCODING",system)
LISPSYM(set_default_file_encoding,"SET-DEFAULT-FILE-ENCODING",system)
#ifdef UNICODE
LISPSYM(pathname_encoding,"PATHNAME-ENCODING",system)
LISPSYM(set_pathname_encoding,"SET-PATHNAME-ENCODING",system)
LISPSYM(terminal_encoding,"TERMINAL-ENCODING",system)
LISPSYM(set_terminal_encoding,"SET-TERMINAL-ENCODING",system)
#if defined(HAVE_FFI) || defined(HAVE_AFFI)
LISPSYM(foreign_encoding,"FOREIGN-ENCODING",system)
LISPSYM(set_foreign_encoding,"SET-FOREIGN-ENCODING",system)
#endif
LISPSYM(misc_encoding,"MISC-ENCODING",system)
LISPSYM(set_misc_encoding,"SET-MISC-ENCODING",system)
#endif
# ---------- ERROR ----------
LISPSYM(error,"ERROR",lisp)
LISPSYM(defclcs,"%DEFCLCS",system)
LISPSYM(cerror_of_type,"CERROR-OF-TYPE",system)
LISPSYM(error_of_type,"ERROR-OF-TYPE",system)
LISPSYM(invoke_debugger,"INVOKE-DEBUGGER",lisp)
LISPSYM(clcs_signal,"SIGNAL",lisp)
# ---------- HASHTABL ----------
LISPSYM(make_hash_table,"MAKE-HASH-TABLE",lisp)
LISPSYM(gethash,"GETHASH",lisp)
LISPSYM(puthash,"PUTHASH",system)
LISPSYM(remhash,"REMHASH",lisp)
LISPSYM(maphash,"MAPHASH",lisp)
LISPSYM(clrhash,"CLRHASH",lisp)
LISPSYM(hash_table_count,"HASH-TABLE-COUNT",lisp)
LISPSYM(hash_table_rehash_size,"HASH-TABLE-REHASH-SIZE",lisp)
LISPSYM(hash_table_rehash_threshold,"HASH-TABLE-REHASH-THRESHOLD",lisp)
LISPSYM(hash_table_size,"HASH-TABLE-SIZE",lisp)
LISPSYM(hash_table_test,"HASH-TABLE-TEST",lisp)
LISPSYM(hash_table_iterator,"HASH-TABLE-ITERATOR",system)
LISPSYM(hash_table_iterate,"HASH-TABLE-ITERATE",system)
LISPSYM(class_gethash,"CLASS-GETHASH",clos)
LISPSYM(class_tuple_gethash,"CLASS-TUPLE-GETHASH",clos)
LISPSYM(sxhash,"SXHASH",lisp)
# ---------- IO ----------
LISPSYM(defio,"%DEFIO",system)
LISPSYM(copy_readtable,"COPY-READTABLE",lisp)
LISPSYM(set_syntax_from_char,"SET-SYNTAX-FROM-CHAR",lisp)
LISPSYM(set_macro_character,"SET-MACRO-CHARACTER",lisp)
LISPSYM(get_macro_character,"GET-MACRO-CHARACTER",lisp)
LISPSYM(make_dispatch_macro_character,"MAKE-DISPATCH-MACRO-CHARACTER",lisp)
LISPSYM(set_dispatch_macro_character,"SET-DISPATCH-MACRO-CHARACTER",lisp)
LISPSYM(get_dispatch_macro_character,"GET-DISPATCH-MACRO-CHARACTER",lisp)
LISPSYM(readtable_case,"READTABLE-CASE",lisp)
LISPSYM(set_readtable_case,"SET-READTABLE-CASE",system)
LISPSYM(lpar_reader,"LPAR-READER",system)
LISPSYM(rpar_reader,"RPAR-READER",system)
LISPSYM(quote_reader,"QUOTE-READER",system)
LISPSYM(function_reader,"FUNCTION-READER",system)
LISPSYM(string_reader,"STRING-READER",system)
LISPSYM(line_comment_reader,"LINE-COMMENT-READER",system)
LISPSYM(comment_reader,"COMMENT-READER",system)
LISPSYM(char_reader,"CHAR-READER",system)
LISPSYM(binary_reader,"BINARY-READER",system)
LISPSYM(octal_reader,"OCTAL-READER",system)
LISPSYM(hexadecimal_reader,"HEXADECIMAL-READER",system)
LISPSYM(radix_reader,"RADIX-READER",system)
LISPSYM(complex_reader,"COMPLEX-READER",system)
LISPSYM(uninterned_reader,"UNINTERNED-READER",system)
LISPSYM(bit_vector_reader,"BIT-VECTOR-READER",system)
LISPSYM(vector_reader,"VECTOR-READER",system)
LISPSYM(array_reader,"ARRAY-READER",system)
LISPSYM(read_eval_reader,"READ-EVAL-READER",system)
LISPSYM(load_eval_reader,"LOAD-EVAL-READER",system)
LISPSYM(label_definition_reader,"LABEL-DEFINIION-READER",system)
LISPSYM(label_reference_reader,"LABEL-REFERENCE-READER",system)
LISPSYM(not_readable_reader,"NOT-READABLE-READER",system)
LISPSYM(syntax_error_reader,"SYNTAX-ERROR-READER",system)
LISPSYM(feature_reader,"FEATURE-READER",system)
LISPSYM(not_feature_reader,"NOT-FEATURE-READER",system)
LISPSYM(structure_reader,"STRUCTURE-READER",system)
LISPSYM(closure_reader,"CLOSURE-READER",system)
LISPSYM(clisp_pathname_reader,"CLISP-PATHNAME-READER",system)
LISPSYM(ansi_pathname_reader,"ANSI-PATHNAME-READER",system)
#ifdef UNIX
LISPSYM(unix_executable_reader,"UNIX-EXECUTABLE-READER",system)
#endif
LISPSYM(read,"READ",lisp)
LISPSYM(read_preserving_whitespace,"READ-PRESERVING-WHITESPACE",lisp)
LISPSYM(read_delimited_list,"READ-DELIMITED-LIST",lisp)
LISPSYM(read_line,"READ-LINE",lisp)
LISPSYM(read_char,"READ-CHAR",lisp)
LISPSYM(unread_char,"UNREAD-CHAR",lisp)
LISPSYM(peek_char,"PEEK-CHAR",lisp)
LISPSYM(listen,"LISTEN",lisp)
LISPSYM(read_char_will_hang_p,"READ-CHAR-WILL-HANG-P",lisp)
LISPSYM(read_char_no_hang,"READ-CHAR-NO-HANG",lisp)
LISPSYM(clear_input,"CLEAR-INPUT",lisp)
LISPSYM(read_from_string,"READ-FROM-STRING",lisp)
LISPSYM(parse_integer,"PARSE-INTEGER",lisp)
LISPSYM(print_structure,"PRINT-STRUCTURE",system)
LISPSYM(write,"WRITE",lisp)
LISPSYM(prin1,"PRIN1",lisp)
LISPSYM(print,"PRINT",lisp)
LISPSYM(pprint,"PPRINT",lisp)
LISPSYM(princ,"PRINC",lisp)
LISPSYM(write_to_string,"WRITE-TO-STRING",lisp)
LISPSYM(prin1_to_string,"PRIN1-TO-STRING",lisp)
LISPSYM(princ_to_string,"PRINC-TO-STRING",lisp)
LISPSYM(write_char,"WRITE-CHAR",lisp)
LISPSYM(write_string,"WRITE-STRING",lisp)
LISPSYM(write_line,"WRITE-LINE",lisp)
LISPSYM(terpri,"TERPRI",lisp)
LISPSYM(fresh_line,"FRESH-LINE",lisp)
LISPSYM(finish_output,"FINISH-OUTPUT",lisp)
LISPSYM(force_output,"FORCE-OUTPUT",lisp)
LISPSYM(clear_output,"CLEAR-OUTPUT",lisp)
LISPSYM(write_unreadable,"WRITE-UNREADABLE",system)
LISPSYM(line_position,"LINE-POSITION",system)
# ---------- LIST ----------
LISPSYM(car,"CAR",lisp)
LISPSYM(cdr,"CDR",lisp)
LISPSYM(caar,"CAAR",lisp)
LISPSYM(cadr,"CADR",lisp)
LISPSYM(cdar,"CDAR",lisp)
LISPSYM(cddr,"CDDR",lisp)
LISPSYM(caaar,"CAAAR",lisp)
LISPSYM(caadr,"CAADR",lisp)
LISPSYM(cadar,"CADAR",lisp)
LISPSYM(caddr,"CADDR",lisp)
LISPSYM(cdaar,"CDAAR",lisp)
LISPSYM(cdadr,"CDADR",lisp)
LISPSYM(cddar,"CDDAR",lisp)
LISPSYM(cdddr,"CDDDR",lisp)
LISPSYM(caaaar,"CAAAAR",lisp)
LISPSYM(caaadr,"CAAADR",lisp)
LISPSYM(caadar,"CAADAR",lisp)
LISPSYM(caaddr,"CAADDR",lisp)
LISPSYM(cadaar,"CADAAR",lisp)
LISPSYM(cadadr,"CADADR",lisp)
LISPSYM(caddar,"CADDAR",lisp)
LISPSYM(cadddr,"CADDDR",lisp)
LISPSYM(cdaaar,"CDAAAR",lisp)
LISPSYM(cdaadr,"CDAADR",lisp)
LISPSYM(cdadar,"CDADAR",lisp)
LISPSYM(cdaddr,"CDADDR",lisp)
LISPSYM(cddaar,"CDDAAR",lisp)
LISPSYM(cddadr,"CDDADR",lisp)
LISPSYM(cdddar,"CDDDAR",lisp)
LISPSYM(cddddr,"CDDDDR",lisp)
LISPSYM(cons,"CONS",lisp)
LISPSYM(tree_equal,"TREE-EQUAL",lisp)
LISPSYM(endp,"ENDP",lisp)
LISPSYM(list_length,"LIST-LENGTH",lisp)
LISPSYM(nth,"NTH",lisp)
LISPSYM(first,"FIRST",lisp)
LISPSYM(second,"SECOND",lisp)
LISPSYM(third,"THIRD",lisp)
LISPSYM(fourth,"FOURTH",lisp)
LISPSYM(fifth,"FIFTH",lisp)
LISPSYM(sixth,"SIXTH",lisp)
LISPSYM(seventh,"SEVENTH",lisp)
LISPSYM(eighth,"EIGHTH",lisp)
LISPSYM(ninth,"NINTH",lisp)
LISPSYM(tenth,"TENTH",lisp)
LISPSYM(rest,"REST",lisp)
LISPSYM(nthcdr,"NTHCDR",lisp)
LISPSYM(last,"LAST",lisp)
LISPSYM(list,"LIST",lisp)
LISPSYM(liststern,"LIST*",lisp)
LISPSYM(make_list,"MAKE-LIST",lisp)
LISPSYM(append,"APPEND",lisp)
LISPSYM(copy_list,"COPY-LIST",lisp)
LISPSYM(copy_alist,"COPY-ALIST",lisp)
LISPSYM(copy_tree,"COPY-TREE",lisp)
LISPSYM(revappend,"REVAPPEND",lisp)
LISPSYM(nconc,"NCONC",lisp)
LISPSYM(nreconc,"NRECONC",lisp)
LISPSYM(list_nreverse,"LIST-NREVERSE",system)
LISPSYM(butlast,"BUTLAST",lisp)
LISPSYM(nbutlast,"NBUTLAST",lisp)
LISPSYM(ldiff,"LDIFF",lisp)
LISPSYM(rplaca,"RPLACA",lisp)
LISPSYM(prplaca,"%RPLACA",system)
LISPSYM(rplacd,"RPLACD",lisp)
LISPSYM(prplacd,"%RPLACD",system)
LISPSYM(subst,"SUBST",lisp)
LISPSYM(subst_if,"SUBST-IF",lisp)
LISPSYM(subst_if_not,"SUBST-IF-NOT",lisp)
LISPSYM(nsubst,"NSUBST",lisp)
LISPSYM(nsubst_if,"NSUBST-IF",lisp)
LISPSYM(nsubst_if_not,"NSUBST-IF-NOT",lisp)
LISPSYM(sublis,"SUBLIS",lisp)
LISPSYM(nsublis,"NSUBLIS",lisp)
LISPSYM(member,"MEMBER",lisp)
LISPSYM(member_if,"MEMBER-IF",lisp)
LISPSYM(member_if_not,"MEMBER-IF-NOT",lisp)
LISPSYM(tailp,"TAILP",lisp)
LISPSYM(adjoin,"ADJOIN",lisp)
LISPSYM(acons,"ACONS",lisp)
LISPSYM(pairlis,"PAIRLIS",lisp)
LISPSYM(assoc,"ASSOC",lisp)
LISPSYM(assoc_if,"ASSOC-IF",lisp)
LISPSYM(assoc_if_not,"ASSOC-IF-NOT",lisp)
LISPSYM(rassoc,"RASSOC",lisp)
LISPSYM(rassoc_if,"RASSOC-IF",lisp)
LISPSYM(rassoc_if_not,"RASSOC-IF-NOT",lisp)
LISPSYM(list_upd,"LIST-UPD",system)
LISPSYM(list_endtest,"LIST-ENDTEST",system)
LISPSYM(list_fe_init,"LIST-FE-INIT",system)
LISPSYM(list_access,"LIST-ACCESS",system)
LISPSYM(list_access_set,"LIST-ACCESS-SET",system)
LISPSYM(list_llength,"LIST-LLENGTH",system)
LISPSYM(list_elt,"LIST-ELT",system)
LISPSYM(list_set_elt,"LIST-SET-ELT",system)
LISPSYM(list_init_start,"LIST-INIT-START",system)
LISPSYM(list_fe_init_end,"LIST-FE-INIT-END",system)
# ---------- MISC ----------
LISPSYM(lisp_implementation_type,"LISP-IMPLEMENTATION-TYPE",lisp)
LISPSYM(lisp_implementation_version,"LISP-IMPLEMENTATION-VERSION",lisp)
LISPSYM(version,"VERSION",system)
#ifdef MACHINE_KNOWN
LISPSYM(machinetype,"MACHINE-TYPE",lisp)
LISPSYM(machine_version,"MACHINE-VERSION",lisp)
#endif
#ifdef HAVE_ENVIRONMENT
LISPSYM(get_env,"GETENV",system)
#endif
#ifdef WIN32_NATIVE
LISPSYM(registry,"REGISTRY",system)
#endif
LISPSYM(software_type,"SOFTWARE-TYPE",lisp)
LISPSYM(software_version,"SOFTWARE-VERSION",lisp)
LISPSYM(current_language,"CURRENT-LANGUAGE",system)
LISPSYM(language,"LANGUAGE",system)
LISPSYM(identity,"IDENTITY",lisp)
LISPSYM(address_of,"ADDRESS-OF",system)
#ifdef HAVE_DISASSEMBLER
LISPSYM(code_address_of,"CODE-ADDRESS-OF",system)
LISPSYM(program_id,"PROGRAM-ID",system)
#endif
LISPSYM(ansi,"ANSI",system)
LISPSYM(set_ansi,"SET-ANSI",system)
# ---------- SOCKET ----------
#ifdef MACHINE_KNOWN
LISPSYM(machine_instance,"MACHINE-INSTANCE",lisp)
#endif
#ifdef HAVE_GETHOSTBYNAME
LISPSYM(socket_service_port,"SOCKET-SERVICE-PORT",lisp)
#ifdef EXPORT_SYSCALLS
LISPSYM(resolve_host_ipaddr,"RESOLVE-HOST-IPADDR",lisp)
#endif
#endif
# ---------- TIME ----------
LISPSYM(get_universal_time,"GET-UNIVERSAL-TIME",lisp)
#if defined(UNIX) || defined(WIN32)
LISPSYM(default_time_zone,"DEFAULT-TIME-ZONE",system)
#endif
LISPSYM(get_internal_run_time,"GET-INTERNAL-RUN-TIME",lisp)
LISPSYM(get_internal_real_time,"GET-INTERNAL-REAL-TIME",lisp)
LISPSYM(sleep,"%SLEEP",system)
LISPSYM(time,"%%TIME",system)
# ---------- PACKAGE ----------
LISPSYM(make_symbol,"MAKE-SYMBOL",lisp)
LISPSYM(find_package,"FIND-PACKAGE",lisp)
LISPSYM(pfind_package,"%FIND-PACKAGE",system)
LISPSYM(package_name,"PACKAGE-NAME",lisp)
LISPSYM(package_nicknames,"PACKAGE-NICKNAMES",lisp)
LISPSYM(rename_package,"RENAME-PACKAGE",lisp)
LISPSYM(package_use_list,"PACKAGE-USE-LIST",lisp)
LISPSYM(package_used_by_list,"PACKAGE-USED-BY-LIST",lisp)
LISPSYM(package_shadowing_symbols,"PACKAGE-SHADOWING-SYMBOLS",lisp)
LISPSYM(list_all_packages,"LIST-ALL-PACKAGES",lisp)
LISPSYM(intern,"INTERN",lisp)
LISPSYM(find_symbol,"FIND-SYMBOL",lisp)
LISPSYM(unintern,"UNINTERN",lisp)
LISPSYM(export,"EXPORT",lisp)
LISPSYM(unexport,"UNEXPORT",lisp)
LISPSYM(import,"IMPORT",lisp)
LISPSYM(shadowing_import,"SHADOWING-IMPORT",lisp)
LISPSYM(shadow,"SHADOW",lisp)
LISPSYM(use_package,"USE-PACKAGE",lisp)
LISPSYM(unuse_package,"UNUSE-PACKAGE",lisp)
LISPSYM(make_package,"MAKE-PACKAGE",lisp)
LISPSYM(pin_package,"%IN-PACKAGE",system)
LISPSYM(in_package,"IN-PACKAGE",lisp)
LISPSYM(delete_package,"DELETE-PACKAGE",lisp)
LISPSYM(find_all_symbols,"FIND-ALL-SYMBOLS",lisp)
LISPSYM(map_symbols,"MAP-SYMBOLS",system)
LISPSYM(map_external_symbols,"MAP-EXTERNAL-SYMBOLS",system)
LISPSYM(map_all_symbols,"MAP-ALL-SYMBOLS",system)
LISPSYM(package_iterator,"PACKAGE-ITERATOR",system)
LISPSYM(package_iterate,"PACKAGE-ITERATE",system)
# ---------- PATHNAME ----------
LISPSYM(parse_namestring,"PARSE-NAMESTRING",lisp)
LISPSYM(pathname,"PATHNAME",lisp)
LISPSYM(pathnamehost,"PATHNAME-HOST",lisp)
LISPSYM(pathnamedevice,"PATHNAME-DEVICE",lisp)
LISPSYM(pathnamedirectory,"PATHNAME-DIRECTORY",lisp)
LISPSYM(pathnamename,"PATHNAME-NAME",lisp)
LISPSYM(pathnametype,"PATHNAME-TYPE",lisp)
LISPSYM(pathnameversion,"PATHNAME-VERSION",lisp)
#ifdef LOGICAL_PATHNAMES
LISPSYM(logical_pathname,"LOGICAL-PATHNAME",lisp)
LISPSYM(translate_logical_pathname,"TRANSLATE-LOGICAL-PATHNAME",lisp)
#endif
LISPSYM(file_namestring,"FILE-NAMESTRING",lisp)
LISPSYM(directory_namestring,"DIRECTORY-NAMESTRING",lisp)
LISPSYM(host_namestring,"HOST-NAMESTRING",lisp)
LISPSYM(merge_pathnames,"MERGE-PATHNAMES",lisp)
LISPSYM(enough_namestring,"ENOUGH-NAMESTRING",lisp)
LISPSYM(make_pathname,"MAKE-PATHNAME",lisp)
#ifdef LOGICAL_PATHNAMES
LISPSYM(make_logical_pathname,"MAKE-LOGICAL-PATHNAME",system)
#endif
#ifdef USER_HOMEDIR
LISPSYM(user_homedir_pathname,"USER-HOMEDIR-PATHNAME",lisp)
#endif
LISPSYM(wild_pathname_p,"WILD-PATHNAME-P",lisp)
LISPSYM(pathname_match_p,"PATHNAME-MATCH-P",lisp)
LISPSYM(translate_pathname,"TRANSLATE-PATHNAME",lisp)
LISPSYM(namestring,"NAMESTRING",lisp)
LISPSYM(truename,"TRUENAME",lisp)
LISPSYM(probe_file,"PROBE-FILE",lisp)
LISPSYM(probe_directory,"PROBE-DIRECTORY",lisp)
LISPSYM(delete_file,"DELETE-FILE",lisp)
LISPSYM(rename_file,"RENAME-FILE",lisp)
LISPSYM(old_open,"OLD-OPEN",system)
LISPSYM(open,"OPEN",lisp)
LISPSYM(directory,"DIRECTORY",lisp)
LISPSYM(cd,"CD",lisp)
LISPSYM(make_dir,"MAKE-DIR",lisp)
LISPSYM(delete_dir,"DELETE-DIR",lisp)
LISPSYM(ensure_directories_exist,"ENSURE-DIRECTORIES-EXIST",lisp)
LISPSYM(file_write_date,"FILE-WRITE-DATE",lisp)
LISPSYM(file_author,"FILE-AUTHOR",lisp)
#if defined(UNIX) || defined(MSDOS) || defined(AMIGAOS) || defined(RISCOS)
LISPSYM(execute,"EXECUTE",lisp)
#endif
#ifdef HAVE_SHELL
#ifdef WIN32_NATIVE
LISPSYM(shell_name,"SHELL-NAME",system)
#endif
LISPSYM(shell,"SHELL",lisp)
#endif
LISPSYM(savemem,"SAVEMEM",lisp)
#ifdef DYNAMIC_MODULES
LISPSYM(dynload_modules,"DYNLOAD-MODULES",system)
#endif
#ifdef HAVE_DISASSEMBLER
LISPSYM(program_name,"PROGRAM-NAME",system)
#endif
LISPSYM(lib_directory,"LIB-DIRECTORY",system)
#ifdef EXPORT_SYSCALLS
#ifdef UNIX
LISPSYM(user_data,"USER-DATA",lisp)
LISPSYM(file_stat,"FILE-STAT",lisp)
#endif
#endif
# ---------- PREDTYPE ----------
LISPSYM(eq,"EQ",lisp)
LISPSYM(eql,"EQL",lisp)
LISPSYM(equal,"EQUAL",lisp)
LISPSYM(equalp,"EQUALP",lisp)
LISPSYM(consp,"CONSP",lisp)
LISPSYM(atom,"ATOM",lisp)
LISPSYM(symbolp,"SYMBOLP",lisp)
LISPSYM(stringp,"STRINGP",lisp)
LISPSYM(numberp,"NUMBERP",lisp)
LISPSYM(compiled_function_p,"COMPILED-FUNCTION-P",lisp)
LISPSYM(null,"NULL",lisp)
LISPSYM(not,"NOT",lisp)
LISPSYM(closurep,"CLOSUREP",system)
LISPSYM(listp,"LISTP",lisp)
LISPSYM(integerp,"INTEGERP",lisp)
LISPSYM(fixnump,"FIXNUMP",system)
LISPSYM(rationalp,"RATIONALP",lisp)
LISPSYM(floatp,"FLOATP",lisp)
LISPSYM(short_float_p,"SHORT-FLOAT-P",system)
LISPSYM(single_float_p,"SINGLE-FLOAT-P",system)
LISPSYM(double_float_p,"DOUBLE-FLOAT-P",system)
LISPSYM(long_float_p,"LONG-FLOAT-P",system)
LISPSYM(realp,"REALP",lisp)
LISPSYM(complexp,"COMPLEXP",lisp)
LISPSYM(streamp,"STREAMP",lisp)
LISPSYM(built_in_stream_p,"BUILT-IN-STREAM-P",system)
LISPSYM(random_state_p,"RANDOM-STATE-P",lisp)
LISPSYM(readtablep,"READTABLEP",lisp)
LISPSYM(hash_table_p,"HASH-TABLE-P",lisp)
LISPSYM(pathnamep,"PATHNAMEP",lisp)
LISPSYM(logical_pathname_p,"LOGICAL-PATHNAME-P",system)
LISPSYM(characterp,"CHARACTERP",lisp)
LISPSYM(functionp,"FUNCTIONP",lisp)
LISPSYM(generic_function_p,"GENERIC-FUNCTION-P",clos)
LISPSYM(packagep,"PACKAGEP",lisp)
LISPSYM(arrayp,"ARRAYP",lisp)
LISPSYM(simple_array_p,"SIMPLE-ARRAY-P",system)
LISPSYM(bit_vector_p,"BIT-VECTOR-P",lisp)
LISPSYM(vectorp,"VECTORP",lisp)
LISPSYM(simple_vector_p,"SIMPLE-VECTOR-P",lisp)
LISPSYM(simple_string_p,"SIMPLE-STRING-P",lisp)
LISPSYM(simple_bit_vector_p,"SIMPLE-BIT-VECTOR-P",lisp)
LISPSYM(commonp,"COMMONP",lisp)
LISPSYM(type_of,"TYPE-OF",lisp)
LISPSYM(defclos,"%DEFCLOS",clos)
LISPSYM(class_p,"CLASS-P",clos)
LISPSYM(class_of,"CLASS-OF",clos)
LISPSYM(find_class,"FIND-CLASS",clos)
LISPSYM(coerce,"COERCE",lisp)
LISPSYM(coerce_fixnum_char_ansi,"*COERCE-FIXNUM-CHAR-ANSI*",lisp)
LISPSYM(note_new_structure_class,"NOTE-NEW-STRUCTURE-CLASS",system)
LISPSYM(note_new_standard_class,"NOTE-NEW-STANDARD-CLASS",system)
LISPSYM(heap_statistics,"HEAP-STATISTICS",system)
LISPSYM(gc_statistics,"GC-STATISTICS",system)
LISPSYM(list_statistics,"LIST-STATISTICS",system)
LISPSYM(heap_statistics_statistics,"HEAP-STATISTICS-STATISTICS",system)
LISPSYM(gc_statistics_statistics,"GC-STATISTICS-STATISTICS",system)
# ---------- RECORD ----------
LISPSYM(record_ref,"%RECORD-REF",system)
LISPSYM(record_store,"%RECORD-STORE",system)
LISPSYM(record_length,"%RECORD-LENGTH",system)
LISPSYM(pstructure_ref,"%%STRUCTURE-REF",system)
LISPSYM(structure_ref,"%STRUCTURE-REF",system)
LISPSYM(structure_store,"%STRUCTURE-STORE",system)
LISPSYM(make_structure,"%MAKE-STRUCTURE",system)
LISPSYM(copy_structure,"COPY-STRUCTURE",lisp)
LISPSYM(structure_type_p,"%STRUCTURE-TYPE-P",system)
LISPSYM(closure_name,"CLOSURE-NAME",system)
LISPSYM(closure_codevec,"CLOSURE-CODEVEC",system)
LISPSYM(closure_consts,"CLOSURE-CONSTS",system)
LISPSYM(make_code_vector,"MAKE-CODE-VECTOR",system)
LISPSYM(make_closure,"%MAKE-CLOSURE",system)
LISPSYM(copy_generic_function,"%COPY-GENERIC-FUNCTION",system)
LISPSYM(generic_function_effective_method_function,"GENERIC-FUNCTION-EFFECTIVE-METHOD-FUNCTION",system)
LISPSYM(make_load_time_eval,"MAKE-LOAD-TIME-EVAL",system)
LISPSYM(make_symbol_macro,"MAKE-SYMBOL-MACRO",system)
LISPSYM(symbol_macro_p,"SYMBOL-MACRO-P",system)
LISPSYM(symbol_macro_expand,"SYMBOL-MACRO-EXPAND",lisp)
LISPSYM(make_weak_pointer,"MAKE-WEAK-POINTER",lisp)
LISPSYM(weak_pointer_p,"WEAK-POINTER-P",lisp)
LISPSYM(weak_pointer_value,"WEAK-POINTER-VALUE",lisp)
LISPSYM(finalize,"FINALIZE",lisp)
LISPSYM(structure_object_p,"STRUCTURE-OBJECT-P",clos)
LISPSYM(std_instance_p,"STD-INSTANCE-P",clos)
LISPSYM(allocate_std_instance,"ALLOCATE-STD-INSTANCE",clos)
LISPSYM(old_pallocate_instance,"OLD-%ALLOCATE-INSTANCE",clos)
LISPSYM(pallocate_instance,"%ALLOCATE-INSTANCE",clos)
LISPSYM(slot_value,"SLOT-VALUE",clos)
LISPSYM(set_slot_value,"SET-SLOT-VALUE",clos)
LISPSYM(slot_boundp,"SLOT-BOUNDP",clos)
LISPSYM(slot_makunbound,"SLOT-MAKUNBOUND",clos)
LISPSYM(slot_exists_p,"SLOT-EXISTS-P",clos)
LISPSYM(shared_initialize,"%SHARED-INITIALIZE",clos)
LISPSYM(reinitialize_instance,"%REINITIALIZE-INSTANCE",clos)
LISPSYM(initialize_instance,"%INITIALIZE-INSTANCE",clos)
LISPSYM(make_instance,"%MAKE-INSTANCE",clos)
# ---------- SEQUENCE ----------
LISPSYM(sequencep,"SEQUENCEP",system)
LISPSYM(defseq,"%DEFSEQ",system)
LISPSYM(elt,"ELT",lisp)
LISPSYM(setelt,"%SETELT",system)
LISPSYM(subseq,"SUBSEQ",lisp)
LISPSYM(copy_seq,"COPY-SEQ",lisp)
LISPSYM(length,"LENGTH",lisp)
LISPSYM(reverse,"REVERSE",lisp)
LISPSYM(nreverse,"NREVERSE",lisp)
LISPSYM(make_sequence,"MAKE-SEQUENCE",lisp)
LISPSYM(concatenate,"CONCATENATE",lisp)
LISPSYM(map,"MAP",lisp)
LISPSYM(map_into,"MAP-INTO",lisp)
LISPSYM(some,"SOME",lisp)
LISPSYM(every,"EVERY",lisp)
LISPSYM(notany,"NOTANY",lisp)
LISPSYM(notevery,"NOTEVERY",lisp)
LISPSYM(reduce,"REDUCE",lisp)
LISPSYM(fill,"FILL",lisp)
LISPSYM(replace,"REPLACE",lisp)
LISPSYM(remove,"REMOVE",lisp)
LISPSYM(remove_if,"REMOVE-IF",lisp)
LISPSYM(remove_if_not,"REMOVE-IF-NOT",lisp)
LISPSYM(delete,"DELETE",lisp)
LISPSYM(delete_if,"DELETE-IF",lisp)
LISPSYM(delete_if_not,"DELETE-IF-NOT",lisp)
LISPSYM(remove_duplicates,"REMOVE-DUPLICATES",lisp)
LISPSYM(delete_duplicates,"DELETE-DUPLICATES",lisp)
LISPSYM(substitute,"SUBSTITUTE",lisp)
LISPSYM(substitute_if,"SUBSTITUTE-IF",lisp)
LISPSYM(substitute_if_not,"SUBSTITUTE-IF-NOT",lisp)
LISPSYM(nsubstitute,"NSUBSTITUTE",lisp)
LISPSYM(nsubstitute_if,"NSUBSTITUTE-IF",lisp)
LISPSYM(nsubstitute_if_not,"NSUBSTITUTE-IF-NOT",lisp)
LISPSYM(find,"FIND",lisp)
LISPSYM(find_if,"FIND-IF",lisp)
LISPSYM(find_if_not,"FIND-IF-NOT",lisp)
LISPSYM(position,"POSITION",lisp)
LISPSYM(position_if,"POSITION-IF",lisp)
LISPSYM(position_if_not,"POSITION-IF-NOT",lisp)
LISPSYM(count,"COUNT",lisp)
LISPSYM(count_if,"COUNT-IF",lisp)
LISPSYM(count_if_not,"COUNT-IF-NOT",lisp)
LISPSYM(mismatch,"MISMATCH",lisp)
LISPSYM(search,"SEARCH",lisp)
LISPSYM(sort,"SORT",lisp)
LISPSYM(stable_sort,"STABLE-SORT",lisp)
LISPSYM(merge,"MERGE",lisp)
LISPSYM(read_char_sequence,"READ-CHAR-SEQUENCE",lisp)
LISPSYM(write_char_sequence,"WRITE-CHAR-SEQUENCE",lisp)
LISPSYM(read_byte_sequence,"READ-BYTE-SEQUENCE",lisp)
LISPSYM(write_byte_sequence,"WRITE-BYTE-SEQUENCE",lisp)
LISPSYM(sequence_count_ansi,"*SEQUENCE-COUNT-ANSI*",lisp)
# ---------- STREAM ----------
LISPSYM(symbol_stream,"SYMBOL-STREAM",system)
LISPSYM(make_synonym_stream,"MAKE-SYNONYM-STREAM",lisp)
LISPSYM(synonym_stream_p,"SYNONYM-STREAM-P",system)
LISPSYM(synonym_stream_symbol,"SYNONYM-STREAM-SYMBOL",lisp)
LISPSYM(make_broadcast_stream,"MAKE-BROADCAST-STREAM",lisp)
LISPSYM(broadcast_stream_p,"BROADCAST-STREAM-P",system)
LISPSYM(broadcast_stream_streams,"BROADCAST-STREAM-STREAMS",lisp)
LISPSYM(make_concatenated_stream,"MAKE-CONCATENATED-STREAM",lisp)
LISPSYM(concatenated_stream_p,"CONCATENATED-STREAM-P",system)
LISPSYM(concatenated_stream_streams,"CONCATENATED-STREAM-STREAMS",lisp)
LISPSYM(make_two_way_stream,"MAKE-TWO-WAY-STREAM",lisp)
LISPSYM(two_way_stream_p,"TWO-WAY-STREAM-P",system)
LISPSYM(two_way_stream_input_stream,"TWO-WAY-STREAM-INPUT-STREAM",lisp)
LISPSYM(two_way_stream_output_stream,"TWO-WAY-STREAM-OUTPUT-STREAM",lisp)
LISPSYM(make_echo_stream,"MAKE-ECHO-STREAM",lisp)
LISPSYM(echo_stream_p,"ECHO-STREAM-P",system)
LISPSYM(echo_stream_input_stream,"ECHO-STREAM-INPUT-STREAM",lisp)
LISPSYM(echo_stream_output_stream,"ECHO-STREAM-OUTPUT-STREAM",lisp)
LISPSYM(make_string_input_stream,"MAKE-STRING-INPUT-STREAM",lisp)
LISPSYM(string_input_stream_index,"STRING-INPUT-STREAM-INDEX",system)
LISPSYM(make_string_output_stream,"MAKE-STRING-OUTPUT-STREAM",lisp)
LISPSYM(get_output_stream_string,"GET-OUTPUT-STREAM-STRING",lisp)
LISPSYM(make_string_push_stream,"MAKE-STRING-PUSH-STREAM",system)
LISPSYM(string_stream_p,"STRING-STREAM-P",system)
LISPSYM(make_buffered_input_stream,"MAKE-BUFFERED-INPUT-STREAM",lisp)
LISPSYM(buffered_input_stream_index,"BUFFERED-INPUT-STREAM-INDEX",system)
LISPSYM(make_buffered_output_stream,"MAKE-BUFFERED-OUTPUT-STREAM",lisp)
#ifdef GENERIC_STREAMS
LISPSYM(generic_stream_controller,"GENERIC-STREAM-CONTROLLER",lisp)
LISPSYM(make_generic_stream,"MAKE-GENERIC-STREAM",lisp)
LISPSYM(generic_stream_p,"GENERIC-STREAM-P",lisp)
#endif
#ifdef KEYBOARD
LISPSYM(make_keyboard_stream,"MAKE-KEYBOARD-STREAM",system)
#endif
LISPSYM(terminal_raw,"TERMINAL-RAW",system)
#ifdef SCREEN
LISPSYM(make_window,"MAKE-WINDOW",screen)
LISPSYM(window_size,"WINDOW-SIZE",screen)
LISPSYM(window_cursor_position,"WINDOW-CURSOR-POSITION",screen)
LISPSYM(set_window_cursor_position,"SET-WINDOW-CURSOR-POSITION",screen)
LISPSYM(clear_window,"CLEAR-WINDOW",screen)
LISPSYM(clear_window_to_eot,"CLEAR-WINDOW-TO-EOT",screen)
LISPSYM(clear_window_to_eol,"CLEAR-WINDOW-TO-EOL",screen)
LISPSYM(delete_window_line,"DELETE-WINDOW-LINE",screen)
LISPSYM(insert_window_line,"INSERT-WINDOW-LINE",screen)
LISPSYM(highlight_on,"HIGHLIGHT-ON",screen)
LISPSYM(highlight_off,"HIGHLIGHT-OFF",screen)
LISPSYM(window_cursor_on,"WINDOW-CURSOR-ON",screen)
LISPSYM(window_cursor_off,"WINDOW-CURSOR-OFF",screen)
#endif
LISPSYM(file_stream_p,"FILE-STREAM-P",system)
#ifdef PRINTER_AMIGAOS
LISPSYM(make_printer_stream,"MAKE-PRINTER-STREAM",system)
#endif
#ifdef PIPES
LISPSYM(make_pipe_input_stream,"MAKE-PIPE-INPUT-STREAM",lisp)
LISPSYM(make_pipe_output_stream,"MAKE-PIPE-OUTPUT-STREAM",lisp)
#ifdef PIPES2
LISPSYM(make_pipe_io_stream,"MAKE-PIPE-IO-STREAM",lisp)
#endif
#endif
#ifdef X11SOCKETS
LISPSYM(make_x11socket_stream,"MAKE-SOCKET-STREAM",system)
LISPSYM(listen_byte,"LISTEN-BYTE",system)
LISPSYM(read_n_bytes,"READ-N-BYTES",system)
LISPSYM(write_n_bytes,"WRITE-N-BYTES",system)
#endif
#ifdef SOCKET_STREAMS
LISPSYM(socket_server,"SOCKET-SERVER",lisp)
LISPSYM(socket_server_close,"SOCKET-SERVER-CLOSE",lisp)
LISPSYM(socket_server_port,"SOCKET-SERVER-PORT",lisp)
LISPSYM(socket_server_host,"SOCKET-SERVER-HOST",lisp)
LISPSYM(socket_accept,"SOCKET-ACCEPT",lisp)
LISPSYM(socket_wait,"SOCKET-WAIT",lisp)
LISPSYM(socket_connect,"SOCKET-CONNECT",lisp)
LISPSYM(socket_stream_port,"SOCKET-STREAM-PORT",lisp)
LISPSYM(socket_stream_host,"SOCKET-STREAM-HOST",lisp)
LISPSYM(socket_stream_peer,"SOCKET-STREAM-PEER",lisp)
LISPSYM(socket_stream_local,"SOCKET-STREAM-LOCAL",lisp)
#ifndef WIN32_NATIVE
LISPSYM(socket_stream_handle,"SOCKET-STREAM-HANDLE",lisp)
#endif
#endif
LISPSYM(built_in_stream_open_p,"BUILT-IN-STREAM-OPEN-P",system)
LISPSYM(input_stream_p,"INPUT-STREAM-P",lisp)
LISPSYM(output_stream_p,"OUTPUT-STREAM-P",lisp)
LISPSYM(built_in_stream_element_type,"BUILT-IN-STREAM-ELEMENT-TYPE",system)
LISPSYM(built_in_stream_set_element_type,"BUILT-IN-STREAM-SET-ELEMENT-TYPE",system)
LISPSYM(stream_external_format,"STREAM-EXTERNAL-FORMAT",lisp)
LISPSYM(set_stream_external_format,"SET-STREAM-EXTERNAL-FORMAT",system)
LISPSYM(interactive_stream_p,"INTERACTIVE-STREAM-P",lisp)
LISPSYM(built_in_stream_close,"BUILT-IN-STREAM-CLOSE",system)
LISPSYM(read_byte,"READ-BYTE",lisp)
LISPSYM(read_integer,"READ-INTEGER",lisp)
LISPSYM(write_byte,"WRITE-BYTE",lisp)
LISPSYM(write_integer,"WRITE-INTEGER",lisp)
LISPSYM(file_position,"FILE-POSITION",lisp)
LISPSYM(file_length,"FILE-LENGTH",lisp)
LISPSYM(file_string_length,"FILE-STRING-LENGTH",lisp)
LISPSYM(line_number,"LINE-NUMBER",system)
LISPSYM(allow_read_eval,"ALLOW-READ-EVAL",system)
LISPSYM(defgray,"%DEFGRAY",system)
# ---------- SYMBOL ----------
LISPSYM(putd,"%PUTD",system)
LISPSYM(find_subr,"%FIND-SUBR",system)
LISPSYM(proclaim_constant,"%PROCLAIM-CONSTANT",system)
LISPSYM(get,"GET",lisp)
LISPSYM(getf,"GETF",lisp)
LISPSYM(get_properties,"GET-PROPERTIES",lisp)
LISPSYM(putplist,"%PUTPLIST",system)
LISPSYM(put,"%PUT",system)
LISPSYM(remprop,"REMPROP",lisp)
LISPSYM(symbol_package,"SYMBOL-PACKAGE",lisp)
LISPSYM(symbol_plist,"SYMBOL-PLIST",lisp)
LISPSYM(symbol_name,"SYMBOL-NAME",lisp)
LISPSYM(keywordp,"KEYWORDP",lisp)
LISPSYM(special_variable_p,"SPECIAL-VARIABLE-P",system)
LISPSYM(gensym,"GENSYM",lisp)
# ---------- LISPARIT ----------
LISPSYM(decimal_string,"DECIMAL-STRING",system)
LISPSYM(zerop,"ZEROP",lisp)
LISPSYM(plusp,"PLUSP",lisp)
LISPSYM(minusp,"MINUSP",lisp)
LISPSYM(oddp,"ODDP",lisp)
LISPSYM(evenp,"EVENP",lisp)
LISPSYM(gleich,"=",lisp)
LISPSYM(ungleich,"/=",lisp)
LISPSYM(kleiner,"<",lisp)
LISPSYM(groesser,">",lisp)
LISPSYM(klgleich,"<=",lisp)
LISPSYM(grgleich,">=",lisp)
LISPSYM(max,"MAX",lisp)
LISPSYM(min,"MIN",lisp)
LISPSYM(plus,"+",lisp)
LISPSYM(minus,"-",lisp)
LISPSYM(mal,"*",lisp)
LISPSYM(durch,"/",lisp)
LISPSYM(einsplus,"1+",lisp)
LISPSYM(einsminus,"1-",lisp)
LISPSYM(conjugate,"CONJUGATE",lisp)
LISPSYM(gcd,"GCD",lisp)
LISPSYM(xgcd,"XGCD",lisp)
LISPSYM(lcm,"LCM",lisp)
LISPSYM(exp,"EXP",lisp)
LISPSYM(expt,"EXPT",lisp)
LISPSYM(log,"LOG",lisp)
LISPSYM(sqrt,"SQRT",lisp)
LISPSYM(isqrt,"ISQRT",lisp)
LISPSYM(abs,"ABS",lisp)
LISPSYM(phase,"PHASE",lisp)
LISPSYM(signum,"SIGNUM",lisp)
LISPSYM(sin,"SIN",lisp)
LISPSYM(cos,"COS",lisp)
LISPSYM(tan,"TAN",lisp)
LISPSYM(cis,"CIS",lisp)
LISPSYM(asin,"ASIN",lisp)
LISPSYM(acos,"ACOS",lisp)
LISPSYM(atan,"ATAN",lisp)
LISPSYM(sinh,"SINH",lisp)
LISPSYM(cosh,"COSH",lisp)
LISPSYM(tanh,"TANH",lisp)
LISPSYM(asinh,"ASINH",lisp)
LISPSYM(acosh,"ACOSH",lisp)
LISPSYM(atanh,"ATANH",lisp)
LISPSYM(float,"FLOAT",lisp)
LISPSYM(rational,"RATIONAL",lisp)
LISPSYM(rationalize,"RATIONALIZE",lisp)
LISPSYM(numerator,"NUMERATOR",lisp)
LISPSYM(denominator,"DENOMINATOR",lisp)
LISPSYM(floor,"FLOOR",lisp)
LISPSYM(ceiling,"CEILING",lisp)
LISPSYM(truncate,"TRUNCATE",lisp)
LISPSYM(round,"ROUND",lisp)
LISPSYM(mod,"MOD",lisp)
LISPSYM(rem,"REM",lisp)
LISPSYM(ffloor,"FFLOOR",lisp)
LISPSYM(fceiling,"FCEILING",lisp)
LISPSYM(ftruncate,"FTRUNCATE",lisp)
LISPSYM(fround,"FROUND",lisp)
LISPSYM(decode_float,"DECODE-FLOAT",lisp)
LISPSYM(scale_float,"SCALE-FLOAT",lisp)
LISPSYM(float_radix,"FLOAT-RADIX",lisp)
LISPSYM(float_sign,"FLOAT-SIGN",lisp)
LISPSYM(float_digits,"FLOAT-DIGITS",lisp)
LISPSYM(float_precision,"FLOAT-PRECISION",lisp)
LISPSYM(integer_decode_float,"INTEGER-DECODE-FLOAT",lisp)
LISPSYM(complex,"COMPLEX",lisp)
LISPSYM(realpart,"REALPART",lisp)
LISPSYM(imagpart,"IMAGPART",lisp)
LISPSYM(logior,"LOGIOR",lisp)
LISPSYM(logxor,"LOGXOR",lisp)
LISPSYM(logand,"LOGAND",lisp)
LISPSYM(logeqv,"LOGEQV",lisp)
LISPSYM(lognand,"LOGNAND",lisp)
LISPSYM(lognor,"LOGNOR",lisp)
LISPSYM(logandc1,"LOGANDC1",lisp)
LISPSYM(logandc2,"LOGANDC2",lisp)
LISPSYM(logorc1,"LOGORC1",lisp)
LISPSYM(logorc2,"LOGORC2",lisp)
LISPSYM(boole,"BOOLE",lisp)
LISPSYM(lognot,"LOGNOT",lisp)
LISPSYM(logtest,"LOGTEST",lisp)
LISPSYM(logbitp,"LOGBITP",lisp)
LISPSYM(ash,"ASH",lisp)
LISPSYM(logcount,"LOGCOUNT",lisp)
LISPSYM(integer_length,"INTEGER-LENGTH",lisp)
LISPSYM(byte,"BYTE",lisp)
LISPSYM(bytesize,"BYTE-SIZE",lisp)
LISPSYM(byteposition,"BYTE-POSITION",lisp)
LISPSYM(ldb,"LDB",lisp)
LISPSYM(ldb_test,"LDB-TEST",lisp)
LISPSYM(mask_field,"MASK-FIELD",lisp)
LISPSYM(dpb,"DPB",lisp)
LISPSYM(deposit_field,"DEPOSIT-FIELD",lisp)
LISPSYM(random,"RANDOM",lisp)
LISPSYM(make_random_state,"MAKE-RANDOM-STATE",lisp)
LISPSYM(fakultaet,"!",lisp)
LISPSYM(exquo,"EXQUO",lisp)
LISPSYM(long_float_digits,"LONG-FLOAT-DIGITS",lisp)
LISPSYM(set_long_float_digits,"%SET-LONG-FLOAT-DIGITS",system)
LISPSYM(log2,"LOG2",system)
LISPSYM(log10,"LOG10",system)
# ---------- REXX ----------
#ifdef REXX
LISPSYM(rexx_put,"%REXX-PUT",system)
LISPSYM(rexx_wait_input,"%REXX-WAIT-INPUT",system)
LISPSYM(rexx_get,"%REXX-GET",system)
LISPSYM(rexx_reply,"%REXX-REPLY",system)
#endif
# ---------- FOREIGN ----------
#ifdef DYNAMIC_FFI
LISPSYM(validp,"VALIDP",ffi)
LISPSYM(sizeof,"%SIZEOF",ffi)
LISPSYM(bitsizeof,"%BITSIZEOF",ffi)
LISPSYM(lookup_foreign_variable,"LOOKUP-FOREIGN-VARIABLE",ffi)
LISPSYM(foreign_value,"FOREIGN-VALUE",ffi)
LISPSYM(set_foreign_value,"SET-FOREIGN-VALUE",ffi)
LISPSYM(foreign_type,"FOREIGN-TYPE",ffi)
LISPSYM(foreign_size,"FOREIGN-SIZE",ffi)
LISPSYM(element,"%ELEMENT",ffi)
LISPSYM(deref,"%DEREF",ffi)
LISPSYM(slot,"%SLOT",ffi)
LISPSYM(cast,"%CAST",ffi)
LISPSYM(offset,"%OFFSET",ffi)
LISPSYM(lookup_foreign_function,"LOOKUP-FOREIGN-FUNCTION",ffi)
LISPSYM(foreign_call_out,"FOREIGN-CALL-OUT",ffi)
#ifdef AMIGAOS
LISPSYM(foreign_library,"FOREIGN-LIBRARY",ffi)
LISPSYM(foreign_library_variable,"FOREIGN-LIBRARY-VARIABLE",ffi)
LISPSYM(foreign_library_function,"FOREIGN-LIBRARY-FUNCTION",ffi)
#endif
#endif

# Keywords:
LISPSYM(Kallow_other_keys,"ALLOW-OTHER-KEYS",keyword)
LISPSYM(Kadjustable,"ADJUSTABLE",keyword)
LISPSYM(Kelement_type,"ELEMENT-TYPE",keyword)
LISPSYM(Kinitial_element,"INITIAL-ELEMENT",keyword)
LISPSYM(Kinitial_contents,"INITIAL-CONTENTS",keyword)
LISPSYM(Kfill_pointer,"FILL-POINTER",keyword)
LISPSYM(Kdisplaced_to,"DISPLACED-TO",keyword)
LISPSYM(Kdisplaced_index_offset,"DISPLACED-INDEX-OFFSET",keyword)
LISPSYM(Kstart1,"START1",keyword)
LISPSYM(Kend1,"END1",keyword)
LISPSYM(Kstart2,"START2",keyword)
LISPSYM(Kend2,"END2",keyword)
LISPSYM(Kstart,"START",keyword)
LISPSYM(Kend,"END",keyword)
LISPSYM(Kpreserve_whitespace,"PRESERVE-WHITESPACE",keyword)
LISPSYM(Kradix,"RADIX",keyword)
LISPSYM(Kjunk_allowed,"JUNK-ALLOWED",keyword)
LISPSYM(Kcase,"CASE",keyword)
LISPSYM(Klevel,"LEVEL",keyword)
LISPSYM(Klength,"LENGTH",keyword)
LISPSYM(Kgensym,"GENSYM",keyword)
LISPSYM(Kescape,"ESCAPE",keyword)
LISPSYM(Kbase,"BASE",keyword)
LISPSYM(Karray,"ARRAY",keyword)
LISPSYM(Kcircle,"CIRCLE",keyword)
LISPSYM(Kpretty,"PRETTY",keyword)
LISPSYM(Kclosure,"CLOSURE",keyword)
LISPSYM(Kreadably,"READABLY",keyword)
LISPSYM(Kright_margin,"RIGHT-MARGIN",keyword)
LISPSYM(Kstream,"STREAM",keyword)
LISPSYM(Kidentity,"IDENTITY",keyword)
LISPSYM(Ktest,"TEST",keyword)
LISPSYM(Ktest_not,"TEST-NOT",keyword)
LISPSYM(Kkey,"KEY",keyword)
LISPSYM(Knicknames,"NICKNAMES",keyword)
LISPSYM(Kuse,"USE",keyword)
LISPSYM(Kcase_sensitive,"CASE-SENSITIVE",keyword)
LISPSYM(Kupdate,"UPDATE",keyword)
LISPSYM(Kfrom_end,"FROM-END",keyword)
LISPSYM(Kinitial_value,"INITIAL-VALUE",keyword)
LISPSYM(Kcount,"COUNT",keyword)
LISPSYM(Ksize,"SIZE",keyword)
LISPSYM(Krehash_size,"REHASH-SIZE",keyword)
LISPSYM(Krehash_threshold,"REHASH-THRESHOLD",keyword)
LISPSYM(Kdefaults,"DEFAULTS",keyword)
LISPSYM(Kdevice,"DEVICE",keyword)
LISPSYM(Kdirectory,"DIRECTORY",keyword)
LISPSYM(Kname,"NAME",keyword)
LISPSYM(Ktype,"TYPE",keyword)
LISPSYM(Kversion,"VERSION",keyword)
LISPSYM(Khost,"HOST",keyword)
LISPSYM(Kall,"ALL",keyword)
LISPSYM(Kmerge,"MERGE",keyword)
LISPSYM(Kdirection,"DIRECTION",keyword)
LISPSYM(Kif_exists,"IF-EXISTS",keyword)
LISPSYM(Kif_does_not_exist,"IF-DOES-NOT-EXIST",keyword)
LISPSYM(Kexternal_format,"EXTERNAL-FORMAT",keyword)
LISPSYM(Kbuffered,"BUFFERED",keyword)
LISPSYM(Kfull,"FULL",keyword)
LISPSYM(Kabort,"ABORT",keyword)
LISPSYM(Kverbose,"VERBOSE",keyword)
LISPSYM(Kexecute,"EXECUTE",keyword)
LISPSYM(Kcompile_toplevel,"COMPILE-TOPLEVEL",keyword)
LISPSYM(Kload_toplevel,"LOAD-TOPLEVEL",keyword)
LISPSYM(Keof,"EOF",keyword)
LISPSYM(Kinput_available,"INPUT-AVAILABLE",keyword)
LISPSYM(Kwait,"WAIT",keyword)
LISPSYM(Kline_position,"LINE-POSITION",keyword)
LISPSYM(Klittle,"LITTLE",keyword)
LISPSYM(Kbig,"BIG",keyword)
LISPSYM(Kcharset,"CHARSET",keyword)
LISPSYM(Kline_terminator,"LINE-TERMINATOR",keyword)
LISPSYM(Kunix,"UNIX",keyword)
LISPSYM(Kmac,"MAC",keyword)
LISPSYM(Kdos,"DOS",keyword)
LISPSYM(Kinput_error_action,"INPUT-ERROR-ACTION",keyword)
LISPSYM(Koutput_error_action,"OUTPUT-ERROR-ACTION",keyword)
LISPSYM(Kansi_cl,"ANSI-CL",keyword)
#ifdef REXX
LISPSYM(Kresult,"RESULT",keyword)
LISPSYM(Kstring,"STRING",keyword)
LISPSYM(Ktoken,"TOKEN",keyword)
#endif

# sonstige Symbole:
LISPSYM(string_char,"STRING-CHAR",lisp) # als Typ in PREDTYPE
LISPSYM(base_char,"BASE-CHAR",lisp) # als Typ in PREDTYPE
LISPSYM(array_rank_limit,"ARRAY-RANK-LIMIT",lisp) # als Konstante in ARRAY
LISPSYM(array_dimension_limit,"ARRAY-DIMENSION-LIMIT",lisp) # als Konstante in ARRAY
LISPSYM(array_total_size_limit,"ARRAY-TOTAL-SIZE-LIMIT",lisp) # als Konstante in ARRAY
LISPSYM(subtype_integer,"SUBTYPE-INTEGER",system) # als Funktion für ARRAY
LISPSYM(char_cod_limit,"CHAR-CODE-LIMIT",lisp) # als Konstante in CHARSTRG
LISPSYM(base_char_cod_limit,"BASE-CHAR-CODE-LIMIT",lisp) # als Konstante in CHARSTRG
LISPSYM(designator,"DESIGNATOR",lisp) # als Typ für CHARSTRG
LISPSYM(class_slots,"CLASS-SLOTS",clos) # als Funktion für RECORD
LISPSYM(slotdef_location,"SLOTDEF-LOCATION",clos) # als Funktion für RECORD
LISPSYM(slotdef_name,"SLOTDEF-NAME",clos) # als Funktion für RECORD
LISPSYM(structure_object,"STRUCTURE-OBJECT",lisp) # als Typ für RECORD
LISPSYM(class,"CLASS",clos) # als Typ für RECORD
LISPSYM(slot_missing,"SLOT-MISSING",clos) # als Funktion für RECORD
LISPSYM(slot_unbound,"SLOT-UNBOUND",clos) # als Funktion für RECORD
LISPSYM(reinitialize_instance_table,"*REINITIALIZE-INSTANCE-TABLE*",clos) # als Variable für RECORD
LISPSYM(make_instance_table,"*MAKE-INSTANCE-TABLE*",clos) # als Variable für RECORD
LISPSYM(initial_reinitialize_instance,"INITIAL-REINITIALIZE-INSTANCE",clos) # als Funktion für RECORD
LISPSYM(initial_initialize_instance,"INITIAL-INITIALIZE-INSTANCE",clos) # als Funktion für RECORD
LISPSYM(initial_make_instance,"INITIAL-MAKE-INSTANCE",clos) # als Funktion für RECORD
LISPSYM(allocate_instance,"ALLOCATE-INSTANCE",clos) # als Funktion für RECORD
LISPSYM(simple_vector,"SIMPLE-VECTOR",lisp) # als Typ in SEQUENCE, PREDTYPE
LISPSYM(simple_string,"SIMPLE-STRING",lisp) # als Typ in SEQUENCE, PREDTYPE
LISPSYM(base_string,"BASE-STRING",lisp) # als Typ in SEQUENCE, PREDTYPE
LISPSYM(simple_base_string,"SIMPLE-BASE-STRING",lisp) # als Typ in SEQUENCE, PREDTYPE
LISPSYM(bit_vector,"BIT-VECTOR",lisp) # als Typ in SEQUENCE, PREDTYPE
LISPSYM(simple_bit_vector,"SIMPLE-BIT-VECTOR",lisp) # als Typ in SEQUENCE, PREDTYPE
LISPSYM(array,"ARRAY",lisp) # als Typ in SEQUENCE, PREDTYPE
LISPSYM(simple_array,"SIMPLE-ARRAY",lisp) # als Typ in SEQUENCE, PREDTYPE
LISPSYM(sequence,"SEQUENCE",lisp) # als Typ für SEQUENCE
LISPSYM(package_error,"PACKAGE-ERROR",lisp) # als Typ für PACKAGE
LISPSYM(Kinternal,"INTERNAL",keyword) # als INTERN-Ergebnis in PACKAGE
LISPSYM(Kexternal,"EXTERNAL",keyword) # als INTERN-Ergebnis in PACKAGE
LISPSYM(Kinherited,"INHERITED",keyword) # als INTERN-Ergebnis in PACKAGE
LISPSYM(do_symbols,"DO-SYMBOLS",lisp) # als Fehlermelder in PACKAGE
LISPSYM(do_external_symbols,"DO-EXTERNAL-SYMBOLS",lisp) # als Fehlermelder in PACKAGE
LISPSYM(packagestern,"*PACKAGE*",lisp) # als Variable in PACKAGE
LISPSYM(internal_time_units_per_second,"INTERNAL-TIME-UNITS-PER-SECOND",lisp) # als Konstante in TIME
LISPSYM(encode_universal_time,"ENCODE-UNIVERSAL-TIME",lisp) # als Funktion in TIME
LISPSYM(use_clcs,"*USE-CLCS*",system) # als Variable in ERROR
LISPSYM(recursive_error_count,"*RECURSIVE-ERROR-COUNT*",system) # als Variable in ERROR
LISPSYM(error_handler,"*ERROR-HANDLER*",lisp) # als Variable in ERROR
LISPSYM(simple_condition,"SIMPLE-CONDITION",lisp) # als Typ für ERROR                                 --+
LISPSYM(simple_serious_condition,"SIMPLE-SERIOUS-CONDITION",system) # als Typ für ERROR                 |
LISPSYM(simple_error,"SIMPLE-ERROR",lisp) # als Typ für ERROR                                           |
LISPSYM(simple_program_error,"SIMPLE-PROGRAM-ERROR",system) # als Typ für ERROR                         |
LISPSYM(simple_source_program_error,"SIMPLE-SOURCE-PROGRAM-ERROR",system) # als Typ für ERROR           |
LISPSYM(simple_control_error,"SIMPLE-CONTROL-ERROR",system) # als Typ für ERROR                         |
LISPSYM(simple_arithmetic_error,"SIMPLE-ARITHMETIC-ERROR",system) # als Typ für ERROR                   |
LISPSYM(simple_division_by_zero,"SIMPLE-DIVISION-BY-ZERO",system) # als Typ für ERROR                   |
LISPSYM(simple_floating_point_overflow,"SIMPLE-FLOATING-POINT-OVERFLOW",system) # als Typ für ERROR     | Reihenfolge
LISPSYM(simple_floating_point_underflow,"SIMPLE-FLOATING-POINT-UNDERFLOW",system) # als Typ für ERROR   | mit ERROR.D,
LISPSYM(simple_cell_error,"SIMPLE-CELL-ERROR",system) # als Typ für ERROR                               | LISPBIBL.D,
LISPSYM(simple_unbound_variable,"SIMPLE-UNBOUND-VARIABLE",system) # als Typ für ERROR                   | CONDITION.LSP
LISPSYM(simple_undefined_function,"SIMPLE-UNDEFINED-FUNCTION",system) # als Typ für ERROR               | abgestimmt!
LISPSYM(simple_unbound_slot,"SIMPLE-UNBOUND-SLOT",system) # als Typ für ERROR                           |
LISPSYM(simple_type_error,"SIMPLE-TYPE-ERROR",lisp) # als Typ für ERROR                                 |
LISPSYM(simple_keyword_error,"SIMPLE-KEYWORD-ERROR",system) # als Typ für ERROR                         |
LISPSYM(simple_charset_type_error,"SIMPLE-CHARSET-TYPE-ERROR",lisp) # als Typ für ERROR                 |
LISPSYM(simple_package_error,"SIMPLE-PACKAGE-ERROR",system) # als Typ für ERROR                         |
LISPSYM(simple_print_not_readable,"SIMPLE-PRINT-NOT-READABLE",system) # als Typ für ERROR               |
LISPSYM(simple_parse_error,"SIMPLE-PARSE-ERROR",system) # als Typ für ERROR                             |
LISPSYM(simple_stream_error,"SIMPLE-STREAM-ERROR",system) # als Typ für ERROR                           |
LISPSYM(simple_end_of_file,"SIMPLE-END-OF-FILE",system) # als Typ für ERROR                             |
LISPSYM(simple_reader_error,"SIMPLE-READER-ERROR",system) # als Typ für ERROR                           |
LISPSYM(simple_file_error,"SIMPLE-FILE-ERROR",system) # als Typ für ERROR                               |
LISPSYM(simple_os_error,"SIMPLE-OS-ERROR",system) # als Typ für ERROR                                   |
LISPSYM(simple_storage_condition,"SIMPLE-STORAGE-CONDITION",system) # als Typ für ERROR                 |
LISPSYM(simple_warning,"SIMPLE-WARNING",lisp) # als Typ für ERROR                                     --+
LISPSYM(Kinstance,"INSTANCE",keyword) # als make-condition-Argument für ERROR
LISPSYM(Kdatum,"DATUM",keyword) # als make-condition-Argument für ERROR
LISPSYM(Kexpected_type,"EXPECTED-TYPE",keyword) # als make-condition-Argument für ERROR
LISPSYM(Kpackage,"PACKAGE",keyword) # als make-condition-Argument für ERROR
LISPSYM(Kobject,"OBJECT",keyword) # als make-condition-Argument für ERROR
LISPSYM(Kpathname,"PATHNAME",keyword) # als make-condition-Argument für ERROR
LISPSYM(format,"FORMAT",lisp) # als Funktion in ERROR
LISPSYM(debugger_hook,"*DEBUGGER-HOOK*",lisp) # als Variable in ERROR
LISPSYM(coerce_to_condition,"COERCE-TO-CONDITION",system) # als Funktion für ERROR
LISPSYM(cerror,"CERROR",lisp) # als Funktion für ERROR
LISPSYM(break_on_signals,"*BREAK-ON-SIGNALS*",lisp) # als Variable für ERROR
LISPSYM(safe_typep,"SAFE-TYPEP",system) # als Funktion für ERROR
LISPSYM(stream_read_byte,"STREAM-READ-BYTE",lisp) # als Funktion für STREAM
LISPSYM(stream_read_byte_sequence,"STREAM-READ-BYTE-SEQUENCE",lisp) # als Funktion für STREAM
LISPSYM(stream_write_byte,"STREAM-WRITE-BYTE",lisp) # als Funktion für STREAM
LISPSYM(stream_write_byte_sequence,"STREAM-WRITE-BYTE-SEQUENCE",lisp) # als Funktion für STREAM
LISPSYM(stream_read_char,"STREAM-READ-CHAR",lisp) # als Funktion für STREAM
LISPSYM(stream_unread_char,"STREAM-UNREAD-CHAR",lisp) # als Funktion für STREAM
LISPSYM(stream_peek_char,"STREAM-PEEK-CHAR",lisp) # als Funktion für STREAM
LISPSYM(stream_read_char_sequence,"STREAM-READ-CHAR-SEQUENCE",lisp) # als Funktion für STREAM
LISPSYM(stream_write_char,"STREAM-WRITE-CHAR",lisp) # als Funktion für STREAM
LISPSYM(stream_write_char_sequence,"STREAM-WRITE-CHAR-SEQUENCE",lisp) # als Funktion für STREAM
LISPSYM(stream_read_line,"STREAM-READ-LINE",lisp) # als Funktion für STREAM
LISPSYM(stream_read_char_will_hang_p,"STREAM-READ-CHAR-WILL-HANG-P",lisp) # als Funktion für STREAM
LISPSYM(stream_clear_input,"STREAM-CLEAR-INPUT",lisp) # als Funktion für STREAM
LISPSYM(stream_finish_output,"STREAM-FINISH-OUTPUT",lisp) # als Funktion für STREAM
LISPSYM(stream_force_output,"STREAM-FORCE-OUTPUT",lisp) # als Funktion für STREAM
LISPSYM(stream_clear_output,"STREAM-CLEAR-OUTPUT",lisp) # als Funktion für STREAM
LISPSYM(stream_line_column,"STREAM-LINE-COLUMN",lisp) # als Funktion für STREAM
#ifdef GENERIC_STREAMS
LISPSYM(generic_stream_rdch,"GENERIC-STREAM-READ-CHAR",lisp) # als Funktion für STREAM
LISPSYM(generic_stream_pkch,"GENERIC-STREAM-PEEK-CHAR",lisp) # als Funktion für STREAM
LISPSYM(generic_stream_read_char_will_hang_p,"GENERIC-STREAM-READ-CHAR-WILL-HANG-P",lisp) # als Funktion für STREAM
LISPSYM(generic_stream_clear_input,"GENERIC-STREAM-CLEAR-INPUT",lisp) # als Funktion für STREAM
LISPSYM(generic_stream_wrch,"GENERIC-STREAM-WRITE-CHAR",lisp) # als Funktion für STREAM
LISPSYM(generic_stream_wrss,"GENERIC-STREAM-WRITE-STRING",lisp) # als Funktion für STREAM
LISPSYM(generic_stream_finish_output,"GENERIC-STREAM-FINISH-OUTPUT",lisp) # als Funktion für STREAM
LISPSYM(generic_stream_force_output,"GENERIC-STREAM-FORCE-OUTPUT",lisp) # als Funktion für STREAM
LISPSYM(generic_stream_clear_output,"GENERIC-STREAM-CLEAR-OUTPUT",lisp) # als Funktion für STREAM
LISPSYM(generic_stream_rdby,"GENERIC-STREAM-READ-BYTE",lisp) # als Funktion für STREAM
LISPSYM(generic_stream_wrby,"GENERIC-STREAM-WRITE-BYTE",lisp) # als Funktion für STREAM
LISPSYM(generic_stream_close,"GENERIC-STREAM-CLOSE",lisp) # als Funktion für STREAM
#endif
#ifdef KEYBOARD
LISPSYM(Kchar,"CHAR",keyword) # als make-input-character-Argument für STREAM
LISPSYM(Kbits,"BITS",keyword) # als make-input-character-Argument für STREAM
LISPSYM(make_input_character,"MAKE-INPUT-CHARACTER",system) # als Funktion für STREAM
LISPSYM(make_char,"MAKE-CHAR",lisp) # als Funktion für STREAM
LISPSYM(keyboard_input,"*KEYBOARD-INPUT*",lisp) # als Variable in STREAM
#endif
LISPSYM(completion,"COMPLETION",system) # als Funktion in STREAM, für den Fall, dass GNU_READLINE benutzt wird
LISPSYM(terminal_io,"*TERMINAL-IO*",lisp) # als Variable in STREAM
LISPSYM(key_bindings,"*KEY-BINDINGS*",system) # als Variable in STREAM
LISPSYM(query_io,"*QUERY-IO*",lisp) # als Variable in STREAM
LISPSYM(debug_io,"*DEBUG-IO*",lisp) # als Variable in STREAM
LISPSYM(standard_input,"*STANDARD-INPUT*",lisp) # als Variable in STREAM
LISPSYM(standard_output,"*STANDARD-OUTPUT*",lisp) # als Variable in STREAM
LISPSYM(error_output,"*ERROR-OUTPUT*",lisp) # als Variable in STREAM
LISPSYM(trace_output,"*TRACE-OUTPUT*",lisp) # als Variable in STREAM
LISPSYM(reval,"$REVAL",system) # als Slotname in STREAM
LISPSYM(default_pathname_defaults,"*DEFAULT-PATHNAME-DEFAULTS*",lisp) # als Variable in PATHNAME
LISPSYM(merge_pathnames_ansi,"*MERGE-PATHNAMES-ANSI*",lisp) # als Variable in PATHNAME
LISPSYM(print_pathnames_ansi,"*PRINT-PATHNAMES-ANSI*",lisp) # als Variable in PATHNAME
#ifdef LOGICAL_PATHNAMES
LISPSYM(logpathname_translations,"*LOGICAL-PATHNAME-TRANSLATIONS*",system) # als Variable in PATHNAME
#endif
LISPSYM(Kwild,"WILD",keyword) # als Pathname-Komponente in PATHNAME
LISPSYM(Kwild_inferiors,"WILD-INFERIORS",keyword) # als Pathname-Komponente in PATHNAME
LISPSYM(Krelative,"RELATIVE",keyword) # als Pathname-Komponente in PATHNAME
LISPSYM(Kabsolute,"ABSOLUTE",keyword) # als Pathname-Komponente in PATHNAME
#if defined(PATHNAME_MSDOS)
LISPSYM(Kcurrent,"CURRENT",keyword) # als Pathname-Komponente in PATHNAME
#endif
#if defined(PATHNAME_MSDOS) || defined(PATHNAME_AMIGAOS) || defined(PATHNAME_RISCOS)
LISPSYM(Kparent,"PARENT",keyword) # als Pathname-Komponente in PATHNAME
#endif
#ifdef PATHNAME_RISCOS
LISPSYM(Kroot,"ROOT",keyword) # als Pathname-Komponente in PATHNAME
LISPSYM(Khome,"HOME",keyword) # als Pathname-Komponente in PATHNAME
LISPSYM(Kcurrent,"CURRENT",keyword) # als Pathname-Komponente in PATHNAME
LISPSYM(Klibrary,"LIBRARY",keyword) # als Pathname-Komponente in PATHNAME
LISPSYM(Kprevious,"PREVIOUS",keyword) # als Pathname-Komponente in PATHNAME
#endif
LISPSYM(Knewest,"NEWEST",keyword) # als Pathname-Komponente in PATHNAME
LISPSYM(Kcommon,"COMMON",keyword) # als Argument in PATHNAME
# LISPSYM(Klocal,"LOCAL",keyword) # als Argument in PATHNAME
LISPSYM(Kinput,"INPUT",keyword) # als Argument in PATHNAME
LISPSYM(Kinput_immutable,"INPUT-IMMUTABLE",keyword) # als Argument in PATHNAME
LISPSYM(Koutput,"OUTPUT",keyword) # als Argument in PATHNAME
LISPSYM(Kio,"IO",keyword) # als Argument in PATHNAME
LISPSYM(Kprobe,"PROBE",keyword) # als Argument in PATHNAME
LISPSYM(unsigned_byte,"UNSIGNED-BYTE",lisp) # als Argument in PATHNAME
LISPSYM(signed_byte,"SIGNED-BYTE",lisp) # als Argument in PATHNAME
LISPSYM(Kdefault,"DEFAULT",keyword) # als Argument in PATHNAME
LISPSYM(canonicalize_type,"CANONICALIZE-TYPE",system) # als Funktion für PATHNAME
LISPSYM(subtypep,"SUBTYPEP",lisp) # als Funktion für PATHNAME
LISPSYM(Kerror,"ERROR",keyword) # als Argument in PATHNAME, ENCODING
LISPSYM(Knew_version,"NEW-VERSION",keyword) # als Argument in PATHNAME
LISPSYM(Krename,"RENAME",keyword) # als Argument in PATHNAME
LISPSYM(Krename_and_delete,"RENAME-AND-DELETE",keyword) # als Argument in PATHNAME
LISPSYM(Koverwrite,"OVERWRITE",keyword) # als Argument in PATHNAME
LISPSYM(Kappend,"APPEND",keyword) # als Argument in PATHNAME
LISPSYM(Ksupersede,"SUPERSEDE",keyword) # als Argument in PATHNAME
LISPSYM(Kcreate,"CREATE",keyword) # als Argument in PATHNAME
LISPSYM(warn,"WARN",lisp) # als Funktion in STREAM, PATHNAME
LISPSYM(Kignore,"IGNORE",keyword) # als Argument in ENCODING
LISPSYM(with_output_to_string,"WITH-OUTPUT-TO-STRING",lisp) # als Fehlermelder in STREAM
LISPSYM(integer,"INTEGER",lisp) # als Typ in STREAM
LISPSYM(hash_table,"HASH-TABLE",lisp) # als Typ in IO, PREDTYPE
LISPSYM(random_state,"RANDOM-STATE",lisp) # als Typ in IO, PREDTYPE
LISPSYM(reader_error,"READER-ERROR",lisp) # als Typ für IO
LISPSYM(read_base,"*READ-BASE*",lisp) # als Variable in IO
LISPSYM(read_suppress,"*READ-SUPPRESS*",lisp) # als Variable in IO
LISPSYM(read_eval,"*READ-EVAL*",lisp) # als Variable in IO
LISPSYM(readtablestern,"*READTABLE*",lisp) # als Variable in IO
LISPSYM(features,"*FEATURES*",lisp) # als Variable in IO
LISPSYM(read_preserve_whitespace,"*READ-PRESERVE-WHITESPACE*",system) # als Variable in IO
LISPSYM(read_line_number,"*READ-LINE-NUMBER*",system) # als Variable in IO
LISPSYM(read_recursive_p,"*READ-RECURSIVE-P*",system) # als Variable in IO
LISPSYM(read_reference_table,"*READ-REFERENCE-TABLE*",system) # als Variable in IO
LISPSYM(backquote_level,"*BACKQUOTE-LEVEL*",system) # als Variable in IO
LISPSYM(backquote_reader,"`-READER",system) # als Funktion für IO
LISPSYM(comma_reader,",-READER",system) # als Funktion für IO
LISPSYM(compiling,"*COMPILING*",system) # als Variable in IO
LISPSYM(make_byte,"MAKE-BYTE",system) # als Funktion für IO
LISPSYM(Kupcase,"UPCASE",keyword) # als *PRINT-CASE* - Wert in IO
LISPSYM(Kdowncase,"DOWNCASE",keyword) # als *PRINT-CASE* - Wert in IO
LISPSYM(Kcapitalize,"CAPITALIZE",keyword) # als *PRINT-CASE* - Wert in IO
                                        # Must be in the same order as in io.d!
LISPSYM(print_case,"*PRINT-CASE*",lisp) # als Variable in IO ----------+
LISPSYM(print_level,"*PRINT-LEVEL*",lisp) # als Variable in IO         |
LISPSYM(print_length,"*PRINT-LENGTH*",lisp) # als Variable in IO       |
LISPSYM(print_gensym,"*PRINT-GENSYM*",lisp) # als Variable in IO       |
LISPSYM(print_escape,"*PRINT-ESCAPE*",lisp) # als Variable in IO       |
LISPSYM(print_radix,"*PRINT-RADIX*",lisp) # als Variable in IO         |
LISPSYM(print_base,"*PRINT-BASE*",lisp) # als Variable in IO           |
LISPSYM(print_array,"*PRINT-ARRAY*",lisp) # als Variable in IO         |
LISPSYM(print_circle,"*PRINT-CIRCLE*",lisp) # als Variable in IO       |
LISPSYM(print_pretty,"*PRINT-PRETTY*",lisp) # als Variable in IO       |
LISPSYM(print_closure,"*PRINT-CLOSURE*",lisp) # als Variable in IO     |
LISPSYM(print_readably,"*PRINT-READABLY*",lisp) # als Variable in IO   |
LISPSYM(print_right_margin,"*PRINT-RIGHT-MARGIN*",lisp) # -------------+
LISPSYM(print_rpars,"*PRINT-RPARS*",lisp) # als Variable in IO
LISPSYM(print_indent_lists,"*PRINT-INDENT-LISTS*",lisp) # als Variable in IO
LISPSYM(print_circle_table,"*PRINT-CIRCLE-TABLE*",system) # als Variable in IO
LISPSYM(prin_level,"*PRIN-LEVEL*",system) # als Variable in IO
LISPSYM(prin_bqlevel,"*PRIN-BQLEVEL*",system) # als Variable in IO
LISPSYM(prin_stream,"*PRIN-STREAM*",system) # als Variable in IO
LISPSYM(prin_linelength,"*PRIN-LINELENGTH*",system) # als Variable in IO
LISPSYM(prin_l1,"*PRIN-L1*",system) # als Variable in IO
LISPSYM(prin_lm,"*PRIN-LM*",system) # als Variable in IO
LISPSYM(prin_rpar,"*PRIN-RPAR*",system) # als Variable in IO
LISPSYM(prin_jblocks,"*PRIN-JBLOCKS*",system) # als Variable in IO
LISPSYM(prin_jbstrings,"*PRIN-JBSTRINGS*",system) # als Variable in IO
LISPSYM(prin_jbmodus,"*PRIN-JBMODUS*",system) # als Variable in IO
LISPSYM(prin_jblpos,"*PRIN-JBLPOS*",system) # als Variable in IO
LISPSYM(backquote,"BACKQUOTE",system) # als Marker in IO
LISPSYM(splice,"SPLICE",system) # als Marker in IO
LISPSYM(nsplice,"NSPLICE",system) # als Marker in IO
LISPSYM(unquote,"UNQUOTE",system) # als Marker in IO
LISPSYM(structure_print,"STRUCTURE-PRINT",system) # als Property in IO
LISPSYM(defstruct_description,"DEFSTRUCT-DESCRIPTION",system) # als Property in IO
LISPSYM(print_object,"PRINT-OBJECT",clos) # als Funktion für IO
LISPSYM(trace_values,"*TRACE-VALUES*",lisp) # als Variable in EVAL
LISPSYM(setf_function,"SETF-FUNCTION",system) # als Property in EVAL
LISPSYM(lambda,"LAMBDA",lisp) # als Marker in EVAL
LISPSYM(LLoptional,"&OPTIONAL",lisp) # als Lambdalisten-Marker in EVAL
LISPSYM(LLkey,"&KEY",lisp) # als Lambdalisten-Marker in EVAL
LISPSYM(LLallow_other_keys,"&ALLOW-OTHER-KEYS",lisp) # als Lambdalisten-Marker in EVAL
LISPSYM(LLrest,"&REST",lisp) # als Lambdalisten-Marker in EVAL
LISPSYM(LLaux,"&AUX",lisp) # als Lambdalisten-Marker in EVAL
LISPSYM(LLbody,"&BODY",lisp) # als Lambdalisten-Marker in EVAL
LISPSYM(macro,"MACRO",system) # als Marker in EVAL
LISPSYM(special,"SPECIAL",lisp) # als Declaration-Specifier in EVAL
LISPSYM(source,"SOURCE",system) # als Declaration-Specifier in EVAL
LISPSYM(optimize,"OPTIMIZE",lisp) # als Declaration-Specifier in EVAL
LISPSYM(declaration,"DECLARATION",lisp) # als Declaration-Specifier in EVAL
LISPSYM(compile_lambda,"COMPILE-LAMBDA",system) # als Funktion für EVAL
LISPSYM(expand_lambdabody_main,"%EXPAND-LAMBDABODY-MAIN",system) # als Funktion für EVAL
LISPSYM(compile,"COMPILE",lisp) # als Declaration-Specifier und Funktion für EVAL
#ifdef DEBUG_EVAL
LISPSYM(funcall_trace_output,"*FUNCALL-TRACE-OUTPUT*",system) # als Variable in EVAL
#endif
LISPSYM(evalhookstern,"*EVALHOOK*",lisp) # als Variable in EVAL
LISPSYM(applyhookstern,"*APPLYHOOK*",lisp) # als Variable in EVAL
LISPSYM(macroexpand_hook,"*MACROEXPAND-HOOK*",lisp) # als Variable in EVAL
LISPSYM(lambda_parameters_limit,"LAMBDA-PARAMETERS-LIMIT",lisp) # als Konstante in EVAL
LISPSYM(call_arguments_limit,"CALL-ARGUMENTS-LIMIT",lisp) # als Konstante in EVAL
LISPSYM(multiple_values_limit,"MULTIPLE-VALUES-LIMIT",lisp) # als Konstante in EVAL
LISPSYM(jmpbuf_size,"*JMPBUF-SIZE*",system) # als Konstante in EVAL für COMPILER
LISPSYM(big_endian,"*BIG-ENDIAN*",system) # als Konstante in EVAL für COMPILER
LISPSYM(Klambda,"LAMBDA",keyword) # als Marker in EVAL
LISPSYM(keyword,"KEYWORD",lisp) # als Typ für EVAL
LISPSYM(plus2,"++",lisp) # als Variable in DEBUG
LISPSYM(plus3,"+++",lisp) # als Variable in DEBUG
LISPSYM(mal2,"**",lisp) # als Variable in DEBUG
LISPSYM(mal3,"***",lisp) # als Variable in DEBUG
LISPSYM(durch2,"//",lisp) # als Variable in DEBUG
LISPSYM(durch3,"///",lisp) # als Variable in DEBUG
LISPSYM(driverstern,"*DRIVER*",lisp) # als Variable in DEBUG
LISPSYM(break_driver,"*BREAK-DRIVER*",lisp) # als Variable in DEBUG
LISPSYM(break_count,"*BREAK-COUNT*",system) # als Variable in DEBUG
LISPSYM(recurse_count_standard_output,"*RECURSE-COUNT-STANDARD-OUTPUT*",system) # als Variable in DEBUG
LISPSYM(recurse_count_debug_io,"*RECURSE-COUNT-DEBUG-IO*",system) # als Variable in DEBUG
LISPSYM(frame_limit1,"*FRAME-LIMIT1*",system) # als Variable in DEBUG
LISPSYM(frame_limit2,"*FRAME-LIMIT2*",system) # als Variable in DEBUG
LISPSYM(setf,"SETF",lisp) # als Marker in CONTROL
LISPSYM(psetf,"PSETF",lisp) # als Marker in CONTROL
LISPSYM(multiple_value_setf,"MULTIPLE-VALUE-SETF",system) # als Marker in CONTROL
LISPSYM(make_macro_expandercons,"MAKE-MACRO-EXPANDERCONS",system) # als Funktion für CONTROL
LISPSYM(type_for_discrimination,"TYPE-FOR-DISCRIMINATION",system) # als Funktion für CONTROL
LISPSYM(pthe,"%THE",system) # als Funktion für CONTROL
LISPSYM(compile_form,"COMPILE-FORM",system) # als Funktion für CONTROL
LISPSYM(otherwise,"OTHERWISE",lisp) # als Marker in CONTROL
LISPSYM(inline,"INLINE",lisp) # als Declaration-Specifier in CONTROL
LISPSYM(notinline,"NOTINLINE",lisp) # als Declaration-Specifier in CONTROL
LISPSYM(get_funname_symbol,"GET-FUNNAME-SYMBOL",system) # als Funktion für CONTROL
LISPSYM(inlinable,"INLINABLE",system) # als Property in CONTROL
LISPSYM(constant_inline,"CONSTANT-INLINE",lisp) # als Declaration-Specifier in CONTROL
LISPSYM(constant_notinline,"CONSTANT-NOTINLINE",lisp) # als Declaration-Specifier in CONTROL
LISPSYM(constant_inlinable,"CONSTANT-INLINABLE",system) # als Property in CONTROL
LISPSYM(boolean,"BOOLEAN",lisp) # als Typ in PREDTYPE
LISPSYM(symbol,"SYMBOL",lisp) # als Typ in PREDTYPE
LISPSYM(address,"ADDRESS",lisp) # als Typ in PREDTYPE
LISPSYM(file_stream,"FILE-STREAM",lisp) # als Typ in PREDTYPE
LISPSYM(synonym_stream,"SYNONYM-STREAM",lisp) # als Typ in PREDTYPE
LISPSYM(broadcast_stream,"BROADCAST-STREAM",lisp) # als Typ in PREDTYPE
LISPSYM(concatenated_stream,"CONCATENATED-STREAM",lisp) # als Typ in PREDTYPE
LISPSYM(two_way_stream,"TWO-WAY-STREAM",lisp) # als Typ in PREDTYPE
LISPSYM(echo_stream,"ECHO-STREAM",lisp) # als Typ in PREDTYPE
LISPSYM(string_stream,"STRING-STREAM",lisp) # als Typ in PREDTYPE
LISPSYM(stream,"STREAM",lisp) # als Typ in PREDTYPE
LISPSYM(package,"PACKAGE",lisp) # als Typ in PREDTYPE
LISPSYM(readtable,"READTABLE",lisp) # als Typ in PREDTYPE
LISPSYM(special_operator,"SPECIAL-OPERATOR",lisp) # als Typ in PREDTYPE
LISPSYM(load_time_eval,"LOAD-TIME-EVAL",lisp) # als Typ in PREDTYPE
LISPSYM(symbol_macro,"SYMBOL-MACRO",lisp) # als Typ in PREDTYPE
LISPSYM(encoding,"ENCODING",lisp) # als Typ in PREDTYPE
#ifdef FOREIGN
LISPSYM(foreign_pointer,"FOREIGN-POINTER",lisp) # als Typ in PREDTYPE
#endif
#ifdef DYNAMIC_FFI
LISPSYM(foreign_address,"FOREIGN-ADDRESS",lisp) # als Typ in PREDTYPE
LISPSYM(foreign_variable,"FOREIGN-VARIABLE",lisp) # als Typ in PREDTYPE
LISPSYM(foreign_function,"FOREIGN-FUNCTION",lisp) # als Typ in PREDTYPE
#endif
LISPSYM(weak_pointer,"WEAK-POINTER",lisp) # als Typ in PREDTYPE
LISPSYM(finalizer,"FINALIZER",lisp) # als Typ in PREDTYPE
#ifdef YET_ANOTHER_RECORD
LISPSYM(yet_another,"YET-ANOTHER",lisp) # als Typ in PREDTYPE
#endif
LISPSYM(compiled_function,"COMPILED-FUNCTION",lisp) # als Typ in PREDTYPE
LISPSYM(frame_pointer,"FRAME-POINTER",lisp) # als Typ in PREDTYPE
LISPSYM(read_label,"READ-LABEL",lisp) # als Typ in PREDTYPE
LISPSYM(system_internal,"SYSTEM-INTERNAL",lisp) # als Typ in PREDTYPE
LISPSYM(fixnum,"FIXNUM",lisp) # als Typ in PREDTYPE
LISPSYM(bignum,"BIGNUM",lisp) # als Typ in PREDTYPE
LISPSYM(ratio,"RATIO",lisp) # als Typ in PREDTYPE
LISPSYM(short_float,"SHORT-FLOAT",lisp) # als Typ in PREDTYPE
LISPSYM(single_float,"SINGLE-FLOAT",lisp) # als Typ in PREDTYPE
LISPSYM(double_float,"DOUBLE-FLOAT",lisp) # als Typ in PREDTYPE
LISPSYM(long_float,"LONG-FLOAT",lisp) # als Typ in PREDTYPE
LISPSYM(standard_generic_function,"STANDARD-GENERIC-FUNCTION",clos) # als Typ in PREDTYPE
LISPSYM(closclass,"CLOSCLASS",clos) # als Marker in PREDTYPE
LISPSYM(typep,"TYPEP",lisp) # als Funktion für PREDTYPE
LISPSYM(deftype_expander,"DEFTYPE-EXPANDER",system) # als Property in PREDTYPE
LISPSYM(gc_statistics_stern,"*GC-STATISTICS*",system) # als Variable für PREDTYPE
LISPSYM(recurse_count_gc_statistics,"*RECURSE-COUNT-GC-STATISTICS*",system) # als Variable in PREDTYPE
LISPSYM(traced_definition,"TRACED-DEFINITION",system) # als Property in SYMBOL
LISPSYM(gensym_counter,"*GENSYM-COUNTER*",lisp) # als Variable in SYMBOL
LISPSYM(pprint_first_newline,"*PPRINT-FIRST-NEWLINE*",lisp) # io.d:pr_enter_1()
LISPSYM(inhibit_floating_point_underflow,"*INHIBIT-FLOATING-POINT-UNDERFLOW*",system) # als Variable in LISPARIT
LISPSYM(warn_on_floating_point_contagion,"*WARN-ON-FLOATING-POINT-CONTAGION*",lisp)
LISPSYM(floating_point_contagion_ansi,"*FLOATING-POINT-CONTAGION-ANSI*",lisp)
LISPSYM(pi,"PI",lisp) # als Variable in LISPARIT
LISPSYM(number,"NUMBER",lisp) # als Typ für LISPARIT
LISPSYM(real,"REAL",lisp) # als Typ für LISPARIT
LISPSYM(most_positive_fixnum,"MOST-POSITIVE-FIXNUM",lisp) # als Konstante in LISPARIT
LISPSYM(most_negative_fixnum,"MOST-NEGATIVE-FIXNUM",lisp) # als Konstante in LISPARIT
LISPSYM(most_positive_short_float,"MOST-POSITIVE-SHORT-FLOAT",lisp) # als Konstante in LISPARIT
LISPSYM(least_positive_short_float,"LEAST-POSITIVE-SHORT-FLOAT",lisp) # als Konstante in LISPARIT
LISPSYM(least_negative_short_float,"LEAST-NEGATIVE-SHORT-FLOAT",lisp) # als Konstante in LISPARIT
LISPSYM(most_negative_short_float,"MOST-NEGATIVE-SHORT-FLOAT",lisp) # als Konstante in LISPARIT
LISPSYM(most_positive_single_float,"MOST-POSITIVE-SINGLE-FLOAT",lisp) # als Konstante in LISPARIT
LISPSYM(least_positive_single_float,"LEAST-POSITIVE-SINGLE-FLOAT",lisp) # als Konstante in LISPARIT
LISPSYM(least_negative_single_float,"LEAST-NEGATIVE-SINGLE-FLOAT",lisp) # als Konstante in LISPARIT
LISPSYM(most_negative_single_float,"MOST-NEGATIVE-SINGLE-FLOAT",lisp) # als Konstante in LISPARIT
LISPSYM(most_positive_double_float,"MOST-POSITIVE-DOUBLE-FLOAT",lisp) # als Konstante in LISPARIT
LISPSYM(least_positive_double_float,"LEAST-POSITIVE-DOUBLE-FLOAT",lisp) # als Konstante in LISPARIT
LISPSYM(least_negative_double_float,"LEAST-NEGATIVE-DOUBLE-FLOAT",lisp) # als Konstante in LISPARIT
LISPSYM(most_negative_double_float,"MOST-NEGATIVE-DOUBLE-FLOAT",lisp) # als Konstante in LISPARIT
LISPSYM(most_positive_long_float,"MOST-POSITIVE-LONG-FLOAT",lisp) # als Variable in LISPARIT
LISPSYM(least_positive_long_float,"LEAST-POSITIVE-LONG-FLOAT",lisp) # als Variable in LISPARIT
LISPSYM(least_negative_long_float,"LEAST-NEGATIVE-LONG-FLOAT",lisp) # als Variable in LISPARIT
LISPSYM(most_negative_long_float,"MOST-NEGATIVE-LONG-FLOAT",lisp) # als Variable in LISPARIT
LISPSYM(least_positive_normalized_long_float,"LEAST-POSITIVE-NORMALIZED-LONG-FLOAT",lisp) # als Variable in LISPARIT
LISPSYM(least_negative_normalized_long_float,"LEAST-NEGATIVE-NORMALIZED-LONG-FLOAT",lisp) # als Variable in LISPARIT
LISPSYM(short_float_epsilon,"SHORT-FLOAT-EPSILON",lisp) # als Konstante in LISPARIT
LISPSYM(single_float_epsilon,"SINGLE-FLOAT-EPSILON",lisp) # als Konstante in LISPARIT
LISPSYM(double_float_epsilon,"DOUBLE-FLOAT-EPSILON",lisp) # als Konstante in LISPARIT
LISPSYM(long_float_epsilon,"LONG-FLOAT-EPSILON",lisp) # als Variable in LISPARIT
LISPSYM(short_float_negative_epsilon,"SHORT-FLOAT-NEGATIVE-EPSILON",lisp) # als Konstante in LISPARIT
LISPSYM(single_float_negative_epsilon,"SINGLE-FLOAT-NEGATIVE-EPSILON",lisp) # als Konstante in LISPARIT
LISPSYM(double_float_negative_epsilon,"DOUBLE-FLOAT-NEGATIVE-EPSILON",lisp) # als Konstante in LISPARIT
LISPSYM(long_float_negative_epsilon,"LONG-FLOAT-NEGATIVE-EPSILON",lisp) # als Variable in LISPARIT
LISPSYM(default_float_format,"*DEFAULT-FLOAT-FORMAT*",lisp) # als Variable in LISPARIT
LISPSYM(read_default_float_format,"*READ-DEFAULT-FLOAT-FORMAT*",lisp) # als Variable in LISPARIT
LISPSYM(write_float,"WRITE-FLOAT",system) # als Funktion für LISPARIT
LISPSYM(random_state_stern,"*RANDOM-STATE*",lisp) # als Variable in LISPARIT
#ifdef UNICODE
LISPSYM(unicode_16,"UNICODE-16",charset)
LISPSYM(unicode_16_big_endian,"UNICODE-16-BIG-ENDIAN",charset)
LISPSYM(unicode_16_little_endian,"UNICODE-16-LITTLE-ENDIAN",charset)
LISPSYM(unicode_32,"UNICODE-32",charset)
LISPSYM(unicode_32_big_endian,"UNICODE-32-BIG-ENDIAN",charset)
LISPSYM(unicode_32_little_endian,"UNICODE-32-LITTLE-ENDIAN",charset)
LISPSYM(utf_8,"UTF-8",charset)
LISPSYM(java,"JAVA",charset)
LISPSYM(ascii,"ASCII",charset) # ---------------------------+ These must be
LISPSYM(iso8859_1,"ISO-8859-1",charset) #                   | in the same order
LISPSYM(iso8859_2,"ISO-8859-2",charset) #                   | as in encoding.d
LISPSYM(iso8859_3,"ISO-8859-3",charset)
LISPSYM(iso8859_4,"ISO-8859-4",charset)
LISPSYM(iso8859_5,"ISO-8859-5",charset)
LISPSYM(iso8859_6,"ISO-8859-6",charset)
LISPSYM(iso8859_7,"ISO-8859-7",charset)
LISPSYM(iso8859_8,"ISO-8859-8",charset)
LISPSYM(iso8859_9,"ISO-8859-9",charset)
LISPSYM(iso8859_10,"ISO-8859-10",charset)
LISPSYM(iso8859_13,"ISO-8859-13",charset)
LISPSYM(iso8859_14,"ISO-8859-14",charset)
LISPSYM(iso8859_15,"ISO-8859-15",charset)
LISPSYM(koi8_r,"KOI8-R",charset)
LISPSYM(mac_arabic,"MAC-ARABIC",charset)
LISPSYM(mac_centraleurope,"MAC-CENTRAL-EUROPE",charset)
LISPSYM(mac_croatian,"MAC-CROATIAN",charset)
LISPSYM(mac_cyrillic,"MAC-CYRILLIC",charset)
LISPSYM(mac_dingbat,"MAC-DINGBAT",charset)
LISPSYM(mac_greek,"MAC-GREEK",charset)
LISPSYM(mac_hebrew,"MAC-HEBREW",charset)
LISPSYM(mac_iceland,"MAC-ICELAND",charset)
LISPSYM(mac_roman,"MAC-ROMAN",charset)
LISPSYM(mac_romania,"MAC-ROMANIA",charset)
LISPSYM(mac_symbol,"MAC-SYMBOL",charset)
LISPSYM(mac_thai,"MAC-THAI",charset)
LISPSYM(mac_turkish,"MAC-TURKISH",charset)
LISPSYM(mac_ukraine,"MAC-UKRAINE",charset)
LISPSYM(cp437_ms,"CP437",charset)
LISPSYM(cp437_ibm,"CP437-IBM",charset)
LISPSYM(cp737,"CP737",charset)
LISPSYM(cp775,"CP775",charset)
LISPSYM(cp850,"CP850",charset)
LISPSYM(cp852_ms,"CP852",charset)
LISPSYM(cp852_ibm,"CP852-IBM",charset)
LISPSYM(cp855,"CP855",charset)
LISPSYM(cp857,"CP857",charset)
LISPSYM(cp860_ms,"CP860",charset)
LISPSYM(cp860_ibm,"CP860-IBM",charset)
LISPSYM(cp861_ms,"CP861",charset)
LISPSYM(cp861_ibm,"CP861-IBM",charset)
LISPSYM(cp862_ms,"CP862",charset)
LISPSYM(cp862_ibm,"CP862-IBM",charset)
LISPSYM(cp863_ms,"CP863",charset)
LISPSYM(cp863_ibm,"CP863-IBM",charset)
LISPSYM(cp864_ms,"CP864",charset)
LISPSYM(cp864_ibm,"CP864-IBM",charset)
LISPSYM(cp865_ms,"CP865",charset)
LISPSYM(cp865_ibm,"CP865-IBM",charset)
LISPSYM(cp866,"CP866",charset)
LISPSYM(cp869_ms,"CP869",charset)
LISPSYM(cp869_ibm,"CP869-IBM",charset)
LISPSYM(cp874_ms,"CP874",charset)
LISPSYM(cp874_ibm,"CP874-IBM",charset)
LISPSYM(cp1250,"CP1250",charset)
LISPSYM(cp1251,"CP1251",charset)
LISPSYM(cp1252,"CP1252",charset)
LISPSYM(cp1253,"CP1253",charset)
LISPSYM(cp1254,"CP1254",charset)
LISPSYM(cp1255,"CP1255",charset)
LISPSYM(cp1256,"CP1256",charset)
LISPSYM(cp1257,"CP1257",charset)
LISPSYM(cp1258,"CP1258",charset) #                          |
LISPSYM(hp_roman8,"HP-ROMAN8",charset) #                    |
LISPSYM(nextstep,"NEXTSTEP",charset) # ---------------------+
LISPSYM(ucs_2,"UCS-2",charset)
LISPSYM(ucs_4,"UCS-4",charset)
LISPSYM(windows_1250,"WINDOWS-1250",charset)
LISPSYM(windows_1251,"WINDOWS-1251",charset)
LISPSYM(windows_1252,"WINDOWS-1252",charset)
LISPSYM(windows_1253,"WINDOWS-1253",charset)
LISPSYM(windows_1254,"WINDOWS-1254",charset)
LISPSYM(windows_1255,"WINDOWS-1255",charset)
LISPSYM(windows_1256,"WINDOWS-1256",charset)
LISPSYM(windows_1257,"WINDOWS-1257",charset)
LISPSYM(windows_1258,"WINDOWS-1258",charset)
#endif
LISPSYM(english,"ENGLISH",lisp) # als Language für MISC
LISPSYM(init_hooks,"*INIT-HOOKS*",system) # als Variable für SPVW
LISPSYM(quiet,"*QUIET*",system) # als Variable für SPVW
LISPSYM(Klisting,"LISTING",keyword) # als Argument für SPVW
LISPSYM(Koutput_file,"OUTPUT-FILE",keyword) # als Argument für SPVW
LISPSYM(compile_file,"COMPILE-FILE",lisp) # als Funktion für SPVW
LISPSYM(load_compiling,"*LOAD-COMPILING*",lisp) # als Variable für SPVW
LISPSYM(load_verbose,"*LOAD-VERBOSE*",lisp) # als Variable für SPVW
LISPSYM(args,"*ARGS*",lisp) # als Variable in SPVW
LISPSYM(batchmode_errors,"BATCHMODE-ERRORS",system) # als Macro für SPVW
LISPSYM(wait_keypress,"WAIT-KEYPRESS",system) # als Funktion für SPVW
# ---------- FFI ----------
#ifdef DYNAMIC_FFI
# LISPSYM(boolean,"BOOLEAN",ffi)
# LISPSYM(char,"CHAR",ffi)
LISPSYM(uchar,"UCHAR",ffi)
LISPSYM(short,"SHORT",ffi)
LISPSYM(ushort,"USHORT",ffi)
LISPSYM(int,"INT",ffi)
LISPSYM(uint,"UINT",ffi)
LISPSYM(long,"LONG",ffi)
LISPSYM(ulong,"ULONG",ffi)
LISPSYM(uint8,"UINT8",ffi)
LISPSYM(sint8,"SINT8",ffi)
LISPSYM(uint16,"UINT16",ffi)
LISPSYM(sint16,"SINT16",ffi)
LISPSYM(uint32,"UINT32",ffi)
LISPSYM(sint32,"SINT32",ffi)
LISPSYM(uint64,"UINT64",ffi)
LISPSYM(sint64,"SINT64",ffi)
# LISPSYM(single_float,"SINGLE-FLOAT",ffi)
# LISPSYM(double_float,"DOUBLE-FLOAT",ffi)
LISPSYM(c_pointer,"C-POINTER",ffi)
LISPSYM(c_string,"C-STRING",ffi)
LISPSYM(c_struct,"C-STRUCT",ffi)
LISPSYM(c_union,"C-UNION",ffi)
LISPSYM(c_array,"C-ARRAY",ffi)
LISPSYM(c_array_max,"C-ARRAY-MAX",ffi)
LISPSYM(c_function,"C-FUNCTION",ffi)
LISPSYM(c_ptr,"C-PTR",ffi)
LISPSYM(c_ptr_null,"C-PTR-NULL",ffi)
LISPSYM(c_array_ptr,"C-ARRAY-PTR",ffi)
LISPSYM(fv_flag_readonly,"FV-FLAG-READONLY",ffi) # als Konstante in FFI
LISPSYM(fv_flag_malloc_free,"FV-FLAG-MALLOC-FREE",ffi) # als Konstante in FFI
LISPSYM(ff_flag_alloca,"FF-FLAG-ALLOCA",ffi) # als Konstante in FFI
LISPSYM(ff_flag_malloc_free,"FF-FLAG-MALLOC-FREE",ffi) # als Konstante in FFI
LISPSYM(ff_flag_out,"FF-FLAG-OUT",ffi) # als Konstante in FFI
LISPSYM(ff_flag_in_out,"FF-FLAG-IN-OUT",ffi) # als Konstante in FFI
LISPSYM(ff_language_asm,"FF-LANGUAGE-ASM",ffi) # als Konstante in FFI
LISPSYM(ff_language_c,"FF-LANGUAGE-C",ffi) # als Konstante in FFI
LISPSYM(ff_language_ansi_c,"FF-LANGUAGE-ANSI-C",ffi) # als Konstante in FFI
LISPSYM(ff_language_stdcall,"FF-LANGUAGE-STDCALL",ffi) # als Konstante in FFI
LISPSYM(foreign_call_in,"FOREIGN-CALL-IN",ffi) # für Fehlermeldung in FFI
#endif
#ifdef HAVE_AFFI
LISPSYM(affi_libcall,"%LIBCALL",system)
LISPSYM(mem_read,"MEM-READ",system)
LISPSYM(mem_write,"MEM-WRITE",system)
LISPSYM(mem_write_vector,"MEM-WRITE-VECTOR",system)
LISPSYM(affi_nonzerop,"NZERO-POINTER-P",system)
#endif

