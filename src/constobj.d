# Liste aller dem C-Programm bekannten Objekte ("Programmkonstanten")
# Bruno Haible 1990-1999

# Die Symbole sind bereits speziell abgehandelt.
# Es wird eine Tabelle aller sonstigen dem C-Programm bekannten Objekte
# gehalten.

# Der Macro LISPOBJ deklariert ein sonstiges LISP-Objekt.
# LISPOBJ(name,initstring)
# > name: Objekt ist als object_tab.name oder als O(name) ansprechbar
# > initstring: Initialisierungsstring in LISP-Syntax

# Expander für die Deklaration der Objekt-Tabelle:
  #define LISPOBJ_A(name,initstring)  \
    object name;

# Expander für die Initialisierung der Objekt-Tabelle:
  #define LISPOBJ_B(name,initstring)  \
    NIL,
  #define LISPOBJ_C(name,initstring)  \
    initstring,

# Welcher Expander benutzt wird, muss vom Hauptfile aus eingestellt werden.

# Der Macro LISPOBJ_S deklariert einen LISP-String.
# > name: Objekt ist als object_tab.name oder als O(name) ansprechbar
# > initstring: Initialisierungsstring in C-Syntax, darf keine Backslashs
#               enthalten
  #define LISPOBJ_S(name,initstring)  \
    LISPOBJ(name,"\"" initstring "\"")

# Der Macro LISPOBJ_L deklariert ein von language abhängiges LISP-Objekt.
# LISPOBJ_L(name,english_initstring)
# > name: Objekt ist als OL(name) ansprechbar
# > english_initstring: Initialisierungsstring für ENGLISH
  #ifdef LANGUAGE_STATIC
    #define LISPOBJ_L(name,english)  \
      LISPOBJ(name,english)
  #else
    #ifndef GNU_GETTEXT
      #define LISPOBJ_L(name,english)  \
        LISPOBJ(name,english)
    #else # GNU_GETTEXT
      #define LISPOBJ_L(name,english)  \
        LISPOBJ(name,"@"english)
    #endif
  #endif

# Der Macro LISPOBJ_LS deklariert einen von language abhängigen LISP-String.
# LISPOBJ_LS(name,english_initstring)
# > name: Objekt ist als OLS(name) ansprechbar
# > english_initstring: Initialisierungsstring für ENGLISH
# Der Initstring in C-Syntax, darf keine Backslashs enthalten.
  #ifndef GNU_GETTEXT
    #define LISPOBJ_LS(name,english)  \
      LISPOBJ_L(name, "\""english"\"")
  #else # GNU_GETTEXT
    #define LISPOBJ_LS(name,english)  \
      LISPOBJ(name,"\""english"\"")
  #endif

# zu SPVW.D:
  # chained list of all active weak-pointers:
  LISPOBJ(all_weakpointers,"0")
  # Liste aller Finalisierer:
  LISPOBJ(all_finalizers,"0")
  # Während der GC: die Liste der nach der GC zu bearbeitenden Finalisierer:
  LISPOBJ(pending_finalizers,"0")
# zu ENCODING.D:
  # Encodings for which both the charset and the line-terminator matter:
  # The default encoding for file streams, pipe streams, socket streams.
  LISPOBJ(default_file_encoding,".")
  # Encodings for which only the charset matters:
  #ifdef UNICODE
    # The encoding of the C strings compiled into the executable.
    LISPOBJ(internal_encoding,".")
    # The encoding of pathnames on the file system.
    LISPOBJ(pathname_encoding,".")
    # The encoding of the terminal stream.
    LISPOBJ(terminal_encoding,".")
    #if defined(HAVE_FFI) || defined(HAVE_AFFI)
      # The encoding of characters and strings passed through the FFI.
      # Must be 1:1, i.e. one of the nls_* encodings.
      LISPOBJ(foreign_encoding,".")
    #endif
    # The encoding for everything else (environment variables, command-line
    # options etc.)
    LISPOBJ(misc_encoding,".")
  #endif
  LISPOBJ(type_line_terminator,"(MEMBER :DEFAULT :UNIX :MAC :DOS)")
  LISPOBJ(type_input_error_action,"(OR (MEMBER :ERROR :IGNORE) CHARACTER)")
  LISPOBJ(type_output_error_action,"(OR (MEMBER :ERROR :IGNORE) CHARACTER (UNSIGNED-BYTE 8))")
# zu CHARSTRG.D:
  # Bei Änderung der Character-Namen außer CONSTOBJ.D auch
  # CHARSTRG.D, FORMAT.LSP, IMPNOTES.HTML anpassen!
  #ifdef AMIGA_CHARNAMES
    LISPOBJ(charname_0,"\"Null\"")
    LISPOBJ(charname_1,"\"Code1\"")
    LISPOBJ(charname_2,"\"Code2\"")
    LISPOBJ(charname_3,"\"Code3\"")
    LISPOBJ(charname_4,"\"Code4\"")
    LISPOBJ(charname_5,"\"Code5\"")
    LISPOBJ(charname_6,"\"Code6\"")
    LISPOBJ(charname_7,"\"Bell\"")
    LISPOBJ(charname_8,"\"Backspace\"")
    LISPOBJ(charname_9,"\"Tab\"")
    LISPOBJ(charname_10,"\"Newline\"")
    LISPOBJ(charname_11,"\"Vt\"")
    LISPOBJ(charname_12,"\"Page\"")
    LISPOBJ(charname_13,"\"Return\"")
    LISPOBJ(charname_14,"\"So\"")
    LISPOBJ(charname_15,"\"Si\"")
    LISPOBJ(charname_16,"\"Code16\"")
    LISPOBJ(charname_17,"\"Code17\"")
    LISPOBJ(charname_18,"\"Code18\"")
    LISPOBJ(charname_19,"\"Code19\"")
    LISPOBJ(charname_20,"\"Code20\"")
    LISPOBJ(charname_21,"\"Code21\"")
    LISPOBJ(charname_22,"\"Code22\"")
    LISPOBJ(charname_23,"\"Code23\"")
    LISPOBJ(charname_24,"\"Code24\"")
    LISPOBJ(charname_25,"\"Code25\"")
    LISPOBJ(charname_26,"\"Code26\"")
    LISPOBJ(charname_27,"\"Escape\"")
    LISPOBJ(charname_28,"\"Code28\"")
    LISPOBJ(charname_29,"\"Code29\"")
    LISPOBJ(charname_30,"\"Code30\"")
    LISPOBJ(charname_31,"\"Code31\"")
    LISPOBJ(charname_32,"\"Space\"")
    LISPOBJ(charname_127,"\"Delete\"")
    LISPOBJ(charname_7bis,"\"Bel\"")
    LISPOBJ(charname_8bis,"\"Bs\"")
    LISPOBJ(charname_9bis,"\"Ht\"")
    LISPOBJ(charname_10bis,"\"Linefeed\"")
    LISPOBJ(charname_10tris,"\"Lf\"")
    LISPOBJ(charname_12bis,"\"Ff\"")
    LISPOBJ(charname_13bis,"\"Cr\"")
    LISPOBJ(charname_27bis,"\"Esc\"")
    LISPOBJ(charname_127bis,"\"Del\"")
    LISPOBJ(charname_127tris,"\"Rubout\"")
    LISPOBJ(charname_155,"\"Csi\"")
  #endif
  #ifdef MSDOS_CHARNAMES
    # Namen von Characters mit Codes 0,7,...,13,26,27,32,8,10:
    LISPOBJ(charname_0,"\"Null\"")
    LISPOBJ(charname_7,"\"Bell\"")
    LISPOBJ(charname_8,"\"Backspace\"")
    LISPOBJ(charname_9,"\"Tab\"")
    LISPOBJ(charname_10,"\"Newline\"")
    LISPOBJ(charname_11,"\"Code11\"")
    LISPOBJ(charname_12,"\"Page\"")
    LISPOBJ(charname_13,"\"Return\"")
    LISPOBJ(charname_26,"\"Code26\"")
    LISPOBJ(charname_27,"\"Escape\"")
    LISPOBJ(charname_32,"\"Space\"")
    LISPOBJ(charname_8bis,"\"Rubout\"")
    LISPOBJ(charname_10bis,"\"Linefeed\"")
  #endif
  #ifdef WIN32_CHARNAMES
    # Namen von Characters mit Codes 0,7,...,13,26,27,32,8,10:
    LISPOBJ(charname_0,"\"Null\"")
    LISPOBJ(charname_7,"\"Bell\"")
    LISPOBJ(charname_8,"\"Backspace\"")
    LISPOBJ(charname_9,"\"Tab\"")
    LISPOBJ(charname_10,"\"Newline\"")
    LISPOBJ(charname_11,"\"Code11\"")
    LISPOBJ(charname_12,"\"Page\"")
    LISPOBJ(charname_13,"\"Return\"")
    LISPOBJ(charname_26,"\"Code26\"")
    LISPOBJ(charname_27,"\"Escape\"")
    LISPOBJ(charname_32,"\"Space\"")
    LISPOBJ(charname_8bis,"\"Rubout\"")
    LISPOBJ(charname_10bis,"\"Linefeed\"")
  #endif
  #ifdef UNIX_CHARNAMES
    LISPOBJ(charname_0bis,"\"Null\"")
    LISPOBJ(charname_7bis,"\"Bell\"")
    LISPOBJ(charname_8bis,"\"Backspace\"")
    LISPOBJ(charname_9bis,"\"Tab\"")
    LISPOBJ(charname_10bis,"\"Newline\"")
    LISPOBJ(charname_10tris,"\"Linefeed\"")
    LISPOBJ(charname_12bis,"\"Page\"")
    LISPOBJ(charname_13bis,"\"Return\"")
    LISPOBJ(charname_27bis,"\"Escape\"")
    LISPOBJ(charname_32bis,"\"Space\"")
    LISPOBJ(charname_127bis,"\"Rubout\"")
    LISPOBJ(charname_127tris,"\"Delete\"")
    LISPOBJ(charname_0,"\"Nul\"")
    LISPOBJ(charname_1,"\"Soh\"")
    LISPOBJ(charname_2,"\"Stx\"")
    LISPOBJ(charname_3,"\"Etx\"")
    LISPOBJ(charname_4,"\"Eot\"")
    LISPOBJ(charname_5,"\"Enq\"")
    LISPOBJ(charname_6,"\"Ack\"")
    LISPOBJ(charname_7,"\"Bel\"")
    LISPOBJ(charname_8,"\"Bs\"")
    LISPOBJ(charname_9,"\"Ht\"")
    LISPOBJ(charname_10,"\"Nl\"")
    LISPOBJ(charname_11,"\"Vt\"")
    LISPOBJ(charname_12,"\"Np\"")
    LISPOBJ(charname_13,"\"Cr\"")
    LISPOBJ(charname_14,"\"So\"")
    LISPOBJ(charname_15,"\"Si\"")
    LISPOBJ(charname_16,"\"Dle\"")
    LISPOBJ(charname_17,"\"Dc1\"")
    LISPOBJ(charname_18,"\"Dc2\"")
    LISPOBJ(charname_19,"\"Dc3\"")
    LISPOBJ(charname_20,"\"Dc4\"")
    LISPOBJ(charname_21,"\"Nak\"")
    LISPOBJ(charname_22,"\"Syn\"")
    LISPOBJ(charname_23,"\"Etb\"")
    LISPOBJ(charname_24,"\"Can\"")
    LISPOBJ(charname_25,"\"Em\"")
    LISPOBJ(charname_26,"\"Sub\"")
    LISPOBJ(charname_27,"\"Esc\"")
    LISPOBJ(charname_28,"\"Fs\"")
    LISPOBJ(charname_29,"\"Gs\"")
    LISPOBJ(charname_30,"\"Rs\"")
    LISPOBJ(charname_31,"\"Us\"")
    LISPOBJ(charname_32,"\"Sp\"")
    # The proposal to add:
    #  constobj.d (UNIX_CHARNAMES): #\Erik is a synonym for #\Null.
    #  LISPOBJ(charname_0tris,"\"Erik\"") # special "honour" for Mr. Nutgum
    # has been rejected because of a seriousness attack.
    LISPOBJ(charname_127,"\"Del\"")
  #endif
# zu ARRAY.D:
  LISPOBJ(type_vector_with_fill_pointer,"(AND VECTOR (SATISFIES ARRAY-HAS-FILL-POINTER-P))") # Typ für Fehlermeldung
# zu HASHTABL.D:
 #ifdef GENERATIONAL_GC
  LISPOBJ(gc_count,"0")
 #endif
# zu SEQUENCE.D:
  # interne Liste aller definierten Sequence-Typen:
  LISPOBJ(seq_types,"NIL")
  LISPOBJ(type_recognizable_sequence_type,"(SATISFIES SYSTEM::RECOGNIZABLE-SEQUENCE-TYPE-P)") # Typ für Fehlermeldung
  # Keywordpaare für test_start_end (Paare nicht trennen!):
  LISPOBJ(kwpair_start,":START")
  LISPOBJ(kwpair_end,":END")
  LISPOBJ(kwpair_start1,":START1")
  LISPOBJ(kwpair_end1,":END1")
  LISPOBJ(kwpair_start2,":START2")
  LISPOBJ(kwpair_end2,":END2")
# zu PREDTYPE.D:
  # Erkennungszeichen für Klassen, wird von CLOS::%DEFCLOS gefüllt
  LISPOBJ(class_structure_types,"(CLOS::CLASS STRUCTURE-OBJECT)")
  # einige Built-In-Klassen, werden von CLOS::%DEFCLOS gefüllt
  LISPOBJ(class_array,"ARRAY")             # ---+
  LISPOBJ(class_bit_vector,"BIT-VECTOR")   #    |   Reihenfolge
  LISPOBJ(class_character,"CHARACTER")     #    |   mit clos.lsp
  LISPOBJ(class_complex,"COMPLEX")         #    |   abgestimmt!
  LISPOBJ(class_cons,"CONS")
  LISPOBJ(class_float,"FLOAT")
  LISPOBJ(class_function,"FUNCTION")
  LISPOBJ(class_hash_table,"HASH-TABLE")
  LISPOBJ(class_integer,"INTEGER")
  LISPOBJ(class_null,"NULL")
  LISPOBJ(class_package,"PACKAGE")
  LISPOBJ(class_pathname,"PATHNAME")
  #ifdef LOGICAL_PATHNAMES
  LISPOBJ(class_logical_pathname,"LOGICAL-PATHNAME")
  #endif
  LISPOBJ(class_random_state,"RANDOM-STATE")
  LISPOBJ(class_ratio,"RATIO")
  LISPOBJ(class_readtable,"READTABLE")
  LISPOBJ(class_standard_generic_function,"CLOS::STANDARD-GENERIC-FUNCTION")
  LISPOBJ(class_stream,"STREAM")
  LISPOBJ(class_file_stream,"FILE-STREAM")
  LISPOBJ(class_synonym_stream,"SYNONYM-STREAM")
  LISPOBJ(class_broadcast_stream,"BROADCAST-STREAM")
  LISPOBJ(class_concatenated_stream,"CONCATENATED-STREAM")
  LISPOBJ(class_two_way_stream,"TWO-WAY-STREAM")
  LISPOBJ(class_echo_stream,"ECHO-STREAM")
  LISPOBJ(class_string_stream,"STRING-STREAM")
  LISPOBJ(class_string,"STRING")
  LISPOBJ(class_symbol,"SYMBOL")           #    |
  LISPOBJ(class_t,"T")                     #    |
  LISPOBJ(class_vector,"VECTOR")           # ---+
  LISPOBJ(type_designator_character,"(DESIGNATOR CHARACTER)")
  #if (base_char_code_limit < char_code_limit)
  LISPOBJ(type_designator_base_char,"(DESIGNATOR BASE-CHAR)")
  #endif
  LISPOBJ(type_designator_function,"(OR FUNCTION SYMBOL (CONS (EQL SETF) (CONS SYMBOL NULL)) (CONS (EQL LAMBDA)))")
  # Upper bound for the number of structure classes present in the system:
  LISPOBJ(structure_class_count_max,"0")
  # Upper bound for the number of standard classes present in the system:
  LISPOBJ(standard_class_count_max,"0")
  # Built-in-Typen für HEAP-STATISTICS
  LISPOBJ(hs_t,"T")                                 # ---+
  LISPOBJ(hs_cons,"CONS")                           #    |  Reihenfolge
  LISPOBJ(hs_null,"NULL")                           #    |  mit enum_hs_...
  LISPOBJ(hs_symbol,"SYMBOL")                       #    |  in predtype.d
  LISPOBJ(hs_simple_bit_vector,"SIMPLE-BIT-VECTOR") #    |  abgestimmt!
  LISPOBJ(hs_simple_string,"SIMPLE-STRING")
  LISPOBJ(hs_simple_vector,"SIMPLE-VECTOR")
  LISPOBJ(hs_bit_vector,"BIT-VECTOR")
  LISPOBJ(hs_byte_vector,"BYTE-VECTOR")
  LISPOBJ(hs_string,"STRING")
  LISPOBJ(hs_vector,"VECTOR")
  LISPOBJ(hs_simple_array,"SIMPLE-ARRAY")
  LISPOBJ(hs_array,"ARRAY")
  LISPOBJ(hs_standard_generic_function,"CLOS::STANDARD-GENERIC-FUNCTION")
  LISPOBJ(hs_function,"FUNCTION")
  LISPOBJ(hs_file_stream,"FILE-STREAM")
  LISPOBJ(hs_synonym_stream,"SYNONYM-STREAM")
  LISPOBJ(hs_broadcast_stream,"BROADCAST-STREAM")
  LISPOBJ(hs_concatenated_stream,"CONCATENATED-STREAM")
  LISPOBJ(hs_two_way_stream,"TWO-WAY-STREAM")
  LISPOBJ(hs_echo_stream,"ECHO-STREAM")
  LISPOBJ(hs_string_stream,"STRING-STREAM")
  LISPOBJ(hs_stream,"STREAM")
  LISPOBJ(hs_hash_table,"HASH-TABLE")
  LISPOBJ(hs_package,"PACKAGE")
  LISPOBJ(hs_readtable,"READTABLE")
  LISPOBJ(hs_pathname,"PATHNAME")
  #ifdef LOGICAL_PATHNAMES
  LISPOBJ(hs_logical_pathname,"LOGICAL-PATHNAME")
  #endif
  LISPOBJ(hs_random_state,"RANDOM-STATE")
  LISPOBJ(hs_byte,"BYTE")
  LISPOBJ(hs_special_operator,"SPECIAL-OPERATOR")
  LISPOBJ(hs_load_time_eval,"LOAD-TIME-EVAL")
  LISPOBJ(hs_symbol_macro,"SYMBOL-MACRO")
  LISPOBJ(hs_encoding,"ENCODING")
  #ifdef FOREIGN
  LISPOBJ(hs_foreign_pointer,"FOREIGN-POINTER")
  #endif
  #ifdef DYNAMIC_FFI
  LISPOBJ(hs_foreign_address,"FOREIGN-ADDRESS")
  LISPOBJ(hs_foreign_variable,"FOREIGN-VARIABLE")
  LISPOBJ(hs_foreign_function,"FOREIGN-FUNCTION")
  #endif
  LISPOBJ(hs_weakpointer,"WEAK-POINTER")
  LISPOBJ(hs_finalizer,"FINALIZER")
  #ifdef SOCKET_STREAMS
  LISPOBJ(hs_socket_server,"SOCKET-SERVER")
  #endif
  #ifdef YET_ANOTHER_RECORD
  LISPOBJ(hs_yetanother,"YETANOTHER")
  #endif
  LISPOBJ(hs_system_function,"SYSTEM-FUNCTION")
  LISPOBJ(hs_bignum,"BIGNUM")
  LISPOBJ(hs_ratio,"RATIO")
  #ifndef WIDE
  LISPOBJ(hs_single_float,"SINGLE-FLOAT")
  #endif                                            #    |
  LISPOBJ(hs_double_float,"DOUBLE-FLOAT")           #    |
  LISPOBJ(hs_long_float,"LONG-FLOAT")               #    |
  LISPOBJ(hs_complex,"COMPLEX")                     # ---+
  LISPOBJ(gc_statistics_list,"NIL")
# zu PACKAGE.D:
  # interne Liste aller Packages:
  LISPOBJ(all_packages,".")
  # die Keyword-Package:
  LISPOBJ(keyword_package,".")
  # die Charset-Package:
  LISPOBJ(charset_package,".")
  # die Default-Package für *PACKAGE*:
  LISPOBJ(default_package,".")
  # verschiedene Strings und Listen für interaktive Konfliktbehebung:
  LISPOBJ_LS(query_string1,
    /* ENGLISH */ "Please choose:")
  LISPOBJ_S(query_string2,"          ")
  LISPOBJ_S(query_string3,"  --  ")
  LISPOBJ_LS(query_string4,
    /* ENGLISH */ "Please choose one of ~:{~A~:^, ~} .")
  LISPOBJ_S(query_string5,">> ")
  LISPOBJ_LS(unint_string1,
    /* ENGLISH */ "symbol ~A from #<PACKAGE ~A> will become a shadowing symbol")
  LISPOBJ_LS(unint_string2,
    /* ENGLISH */ "You may choose the symbol in favour of which to resolve the conflict.")
  LISPOBJ_LS(unint_string3,
    /* ENGLISH */ "uninterning ~S from ~S uncovers a name conflict.")
  LISPOBJ_LS(import_string1,
    /* ENGLISH */ "You may choose how to proceed.")
  LISPOBJ_LS(import_string2,
    /* ENGLISH */ "importing ~S into ~S produces a name conflict with ~S.")
  LISPOBJ_LS(import_string3,
    /* ENGLISH */ "importing ~S into ~S produces a name conflict with ~S and other symbols.")
  LISPOBJ_L(import_list1,
    /* ENGLISH */ "((\"I\" \"import it and unintern the other symbol\" T)"
                  " (\"N\" \"do not import it, leave undone\" NIL))")
  LISPOBJ_L(import_list2,
    /* ENGLISH */ "((\"I\" \"import it, unintern one other symbol and shadow the other symbols\" T)"
                  " (\"N\" \"do not import it, leave undone\" NIL))")
  LISPOBJ_L(import_list3,
    /* ENGLISH */ "((\"I\" \"import it and shadow the other symbol\" T) (\"N\" \"do nothing\" NIL))")
  LISPOBJ_LS(export_string1,
    /* ENGLISH */ "You may choose how to proceed.")
  LISPOBJ_LS(export_string2,
    /* ENGLISH */ "symbol ~S should be imported into ~S before being exported.")
  LISPOBJ_L(export_list1,
    /* ENGLISH */ "((\"I\" \"import the symbol first\" T)"
                  " (\"N\" \"do nothing, don't export the symbol\" NIL))")
  LISPOBJ_LS(export_string3,
    /* ENGLISH */ "You may choose in favour of which symbol to resolve the conflict.")
  LISPOBJ_LS(export_string4,
    /* ENGLISH */ "exporting ~S from ~S produces a name conflict with ~S from ~S.")
  LISPOBJ_LS(export_string5,
    /* ENGLISH */ "Which symbol should be accessible in ~S ?")
  LISPOBJ_S(export_string6,"1")
  LISPOBJ_S(export_string7,"2")
  LISPOBJ_LS(export_string8,
    /* ENGLISH */ "the symbol to export, ")
  LISPOBJ_LS(export_string9,
    /* ENGLISH */ "the old symbol, ")
  LISPOBJ_LS(usepack_string1,
    /* ENGLISH */ "You may choose for every conflict in favour of which symbol to resolve it.")
  LISPOBJ_LS(usepack_string2,
    /* ENGLISH */ "~S name conflicts while executing USE-PACKAGE of ~S into package ~S.")
  LISPOBJ_LS(usepack_string3,
    /* ENGLISH */ "which symbol with name ~S should be accessible in ~S ?")
  LISPOBJ_LS(makepack_string1,
    /* ENGLISH */ "You can input another name.")
  LISPOBJ_LS(makepack_string2,
    /* ENGLISH */ "You can input another nickname.")
  LISPOBJ_LS(makepack_string3,
    /* ENGLISH */ "a package with name ~S already exists.")
  LISPOBJ_LS(makepack_string4,
    /* ENGLISH */ "Please input new package name:")
  LISPOBJ_LS(makepack_string5,
    /* ENGLISH */ "Please input new package nickname:")
  LISPOBJ_LS(delpack_string1,
    /* ENGLISH */ "Ignore.")
  LISPOBJ_LS(delpack_string2,
    /* ENGLISH */ "~S: There is no package with name ~S.")
  LISPOBJ_LS(delpack_string3,
    /* ENGLISH */ "~*Nevertheless delete ~S.")
  LISPOBJ_LS(delpack_string4,
    /* ENGLISH */ "~S: ~S is used by ~{~S~^, ~}.")
  # Default-Use-List:
  LISPOBJ(use_default,"(\"LISP\" \"CLOS\")")
  # Default-Package bei ANSI-CL-Compliance:
  LISPOBJ(ansi_user_package_name,"\"COMMON-LISP-USER\"")
# zu SYMBOL.D:
  LISPOBJ(gensym_prefix,"\"G\"") # Präfix für gensym, ein String
# zu MISC.D:
  # Eigenwissen:
  LISPOBJ_S(lisp_implementation_type_string,"CLISP")
  LISPOBJ_S(lisp_implementation_version_date_string,VERSION)
  #ifdef VERSION_MM
  #if VERSION_MM==1
  LISPOBJ_LS(lisp_implementation_version_month_string,
    /* ENGLISH */ "January")
  #endif
  #if VERSION_MM==2
  LISPOBJ_LS(lisp_implementation_version_month_string,
    /* ENGLISH */ "February")
  #endif
  #if VERSION_MM==3
  LISPOBJ_LS(lisp_implementation_version_month_string,
    /* ENGLISH */ "March")
  #endif
  #if VERSION_MM==4
  LISPOBJ_LS(lisp_implementation_version_month_string,
    /* ENGLISH */ "April")
  #endif
  #if VERSION_MM==5
  LISPOBJ_LS(lisp_implementation_version_month_string,
    /* ENGLISH */ "May")
  #endif
  #if VERSION_MM==6
  LISPOBJ_LS(lisp_implementation_version_month_string,
    /* ENGLISH */ "June")
  #endif
  #if VERSION_MM==7
  LISPOBJ_LS(lisp_implementation_version_month_string,
    /* ENGLISH */ "July")
  #endif
  #if VERSION_MM==8
  LISPOBJ_LS(lisp_implementation_version_month_string,
    /* ENGLISH */ "August")
  #endif
  #if VERSION_MM==9
  LISPOBJ_LS(lisp_implementation_version_month_string,
    /* ENGLISH */ "September")
  #endif
  #if VERSION_MM==10
  LISPOBJ_LS(lisp_implementation_version_month_string,
    /* ENGLISH */ "October")
  #endif
  #if VERSION_MM==11
  LISPOBJ_LS(lisp_implementation_version_month_string,
    /* ENGLISH */ "November")
  #endif
  #if VERSION_MM==12
  LISPOBJ_LS(lisp_implementation_version_month_string,
    /* ENGLISH */ "December")
  #endif
  #else # [dummy: often version.h is not included]
  LISPOBJ_LS(lisp_implementation_version_month_string,"")
  #endif
  LISPOBJ_S(lisp_implementation_version_year_string,VERSION_YYYY_STRING)
  LISPOBJ(lisp_implementation_version_string,"NIL") # ein Cache
  LISPOBJ(version,"(19990501)") # Date of last change of bytecode interpreter
  LISPOBJ(oldversion,"(19981031)")
  #ifdef MACHINE_KNOWN
    LISPOBJ(machine_type_string,"NIL")
    LISPOBJ(machine_version_string,"NIL")
    LISPOBJ(machine_instance_string,"NIL")
  #endif
  LISPOBJ_LS(software_type_string,
    /* ENGLISH */ "ANSI C program")
 #if defined(GNU)
  #if defined(__cplusplus)
  LISPOBJ_LS(c_compiler_name,
    /* ENGLISH */ "GNU C++ ")
  #else
  LISPOBJ_LS(c_compiler_name,
    /* ENGLISH */ "GNU C ")
  #endif
  LISPOBJ_S(c_compiler_version,__VERSION__)
  LISPOBJ(software_version_string,"NIL") # ein Cache
 #else
  #if defined(__cplusplus)
  LISPOBJ_LS(software_version_string,
    /* ENGLISH */ "C++ compiler")
  #else
  LISPOBJ_LS(software_version_string,
    /* ENGLISH */ "C compiler")
  #endif
 #endif
 #ifdef GNU_GETTEXT
  LISPOBJ_L(current_language,
    /* ENGLISH */ "ENGLISH")
  LISPOBJ(current_language_cache,"NIL")
 #endif
  LISPOBJ(ansi,"NIL")
# zu TIME.D:
 #ifdef TIME_RELATIVE
  # Start-Universal-Time:
  LISPOBJ(start_UT,"NIL")
 #endif
# zu ERROR.D:
  # Errormeldungs-Startstring:
  LISPOBJ_S(error_string1,"*** - ")
  # Vektor mit Conditions und Simple-Conditions:
  LISPOBJ(error_types,"#()")
  # für Errors vom Typ TYPE-ERROR:
  LISPOBJ(type_uint8,"(INTEGER 0 255)") # oder "(UNSIGNED-BYTE 8)"
  LISPOBJ(type_sint8,"(INTEGER -128 127)") # oder "(SIGNED-BYTE 8)"
  LISPOBJ(type_uint16,"(INTEGER 0 65535)") # oder "(UNSIGNED-BYTE 16)"
  LISPOBJ(type_sint16,"(INTEGER -32768 32767)") # oder "(SIGNED-BYTE 16)"
  LISPOBJ(type_uint32,"(INTEGER 0 4294967295)") # oder "(UNSIGNED-BYTE 32)"
  LISPOBJ(type_sint32,"(INTEGER -2147483648 2147483647)") # oder "(SIGNED-BYTE 32)"
  LISPOBJ(type_uint64,"(INTEGER 0 18446744073709551615)") # oder "(UNSIGNED-BYTE 64)"
  LISPOBJ(type_sint64,"(INTEGER -9223372036854775808 9223372036854775807)") # oder "(SIGNED-BYTE 64)"
  LISPOBJ(type_array_index,"(INTEGER 0 (#.ARRAY-DIMENSION-LIMIT))")
  LISPOBJ(type_array_bit,"(ARRAY BIT)")
  LISPOBJ(type_posfixnum,"(INTEGER 0 #.MOST-POSITIVE-FIXNUM)")
  LISPOBJ(type_posfixnum1,"(INTEGER (0) #.MOST-POSITIVE-FIXNUM)")
  LISPOBJ(type_array_rank,"(INTEGER 0 (#.ARRAY-RANK-LIMIT))")
  LISPOBJ(type_radix,"(INTEGER 2 36)")
  LISPOBJ(type_end_index,"(OR NULL INTEGER)")
  LISPOBJ(type_posinteger,"(INTEGER 0 *)")
  LISPOBJ(type_stringsymchar,"(OR STRING SYMBOL CHARACTER)")
  LISPOBJ(type_svector2,"(SIMPLE-VECTOR 2)")
  LISPOBJ(type_svector5,"(SIMPLE-VECTOR 5)")
  LISPOBJ(type_climb_mode,"(INTEGER 1 5)")
  LISPOBJ(type_hashtable_test,"(MEMBER EQ EQL EQUAL EQUALP #.#'EQ #.#'EQL #.#'EQUAL #.#'EQUALP)")
  LISPOBJ(type_hashtable_size,"(INTEGER 0 #.(floor (- most-positive-fixnum 1) 2))")
  LISPOBJ(type_hashtable_rehash_size,"(FLOAT (1.0) *)")
  LISPOBJ(type_hashtable_rehash_threshold,"(FLOAT 0.0 1.0)")
  LISPOBJ(type_boole,"(INTEGER 0 15)")
  LISPOBJ(type_not_digit,"(AND CHARACTER (NOT (SATISFIES DIGIT-CHAR-P)))")
  LISPOBJ(type_rtcase,"(MEMBER :UPCASE :DOWNCASE :PRESERVE :INVERT)")
  LISPOBJ(type_peektype,"(OR BOOLEAN CHARACTER)")
  LISPOBJ(type_printcase,"(MEMBER :UPCASE :DOWNCASE :CAPITALIZE)")
  LISPOBJ(type_random_arg,"(OR (INTEGER (0) *) (FLOAT (0.0) *))")
  LISPOBJ(type_packname,"(OR PACKAGE STRING SYMBOL)")
  LISPOBJ(type_stringsym,"(OR STRING SYMBOL)")
  LISPOBJ(type_posint16,"(INTEGER (0) (65536))")
  LISPOBJ(type_string_integer,"(OR STRING INTEGER)")
  LISPOBJ(type_uint8_vector,"(ARRAY (UNSIGNED-BYTE 8) (*))")
  LISPOBJ(type_position,"(OR (MEMBER :START :END) (INTEGER 0 4294967295))")
 #if HAS_HOST || defined(LOGICAL_PATHNAMES)
  LISPOBJ(type_host,"(OR NULL STRING)")
 #endif
 #if HAS_VERSION || defined(LOGICAL_PATHNAMES)
  LISPOBJ(type_version,"(OR (MEMBER NIL :WILD :NEWEST) (INTEGER (0) #.MOST-POSITIVE-FIXNUM) PATHNAME)")
 #else
  LISPOBJ(type_version,"(MEMBER NIL :WILD :NEWEST)")
 #endif
  LISPOBJ(type_direction,"(MEMBER :INPUT :INPUT-IMMUTABLE :OUTPUT :IO :PROBE)")
  LISPOBJ(type_if_exists,"(MEMBER :ERROR :NEW-VERSION :RENAME :RENAME-AND-DELETE :OVERWRITE :APPEND :SUPERSEDE NIL)")
  LISPOBJ(type_if_does_not_exist,"(MEMBER :ERROR :CREATE NIL)")
  LISPOBJ(type_external_format,"(OR (MEMBER :DEFAULT) ENCODING (MEMBER :UNIX :MAC :DOS))")
  LISPOBJ(type_pathname_field_key,"(MEMBER :HOST :DEVICE :DIRECTORY :NAME :TYPE :VERSION NIL)")
 #ifdef LOGICAL_PATHNAMES
  LISPOBJ(type_logical_pathname,"(OR LOGICAL-PATHNAME STRING STREAM SYMBOL)")
 #endif
  LISPOBJ(type_builtin_stream,"(SATISFIES SYSTEM::BUILT-IN-STREAM-P)")
# zu PATHNAME.D:
  LISPOBJ(type_designator_pathname,"(OR STRING FILE-STREAM PATHNAME)")
 #ifdef LOGICAL_PATHNAMES
  LISPOBJ(empty_logical_pathname,".") # (schon initialisiert)
  LISPOBJ(default_logical_pathname_host,"\"SYS\"")
 #endif
  LISPOBJ_S(leer_string,"")
  LISPOBJ_S(wild_string,"*")
  LISPOBJ_S(doppelpunkt_string,":")
 #if defined(PATHNAME_MSDOS) || defined(PATHNAME_OS2) || defined(PATHNAME_WIN32)
  LISPOBJ(backslash_string,"\"\\\\\"")
 #endif
 #if defined(PATHNAME_UNIX) || defined(PATHNAME_AMIGAOS)
  LISPOBJ_S(slash_string,"/")
 #endif
  LISPOBJ_S(punkt_string,".")
 #if defined(PATHNAME_MSDOS) || defined(PATHNAME_OS2) || defined(PATHNAME_WIN32) || defined(PATHNAME_UNIX) || defined(PATHNAME_AMIGAOS)
  LISPOBJ_S(punktpunkt_string,"..")
  LISPOBJ_S(punktpunktpunkt_string,"...")
 #endif
 #ifdef PATHNAME_RISCOS
  LISPOBJ_S(parent_string,"^")
  LISPOBJ_S(root_string,"$.")
  LISPOBJ_S(home_string,"&.")
  LISPOBJ_S(current_string,"@.")
  LISPOBJ_S(library_string,"%.")
  LISPOBJ(previous_string,"\"\\\\.\"")
 #endif
 #ifdef PATHNAME_OS2
  LISPOBJ(pipe_subdirs,"(\"PIPE\")")
 #endif
 #if defined(PATHNAME_MSDOS) || defined(PATHNAME_OS2)
  LISPOBJ_S(wild_wild_string,"*.*")
 #endif
 #ifdef PATHNAME_MSDOS
  LISPOBJ_S(backuptype_string,"BAK") # Filetyp von Backupfiles
 #endif
 #ifdef PATHNAME_OS2
  LISPOBJ_S(backuptype_string,"bak") # Filetyp von Backupfiles
 #endif
 #if defined(PATHNAME_AMIGAOS) || defined(PATHNAME_WIN32)
  LISPOBJ_S(backupextend_string,".bak") # Namenserweiterung von Backupfiles
 #endif
 #ifdef PATHNAME_UNIX
  LISPOBJ_S(backupextend_string,"%") # Namenserweiterung von Backupfiles
 #endif
 #ifdef PATHNAME_RISCOS
  LISPOBJ_S(backupprepend_string,"~") # Namenserweiterung von Backupfiles
 #endif
 #if defined(PATHNAME_MSDOS) || defined(PATHNAME_OS2) || defined(PATHNAME_WIN32)
  # Default-Drive (als String der Länge 1):
  LISPOBJ(default_drive,"NIL")
 #endif
 #if defined(PATHNAME_UNIX) || defined(PATHNAME_AMIGAOS) || defined(PATHNAME_OS2) || defined(PATHNAME_WIN32)
  LISPOBJ_S(wildwild_string,"**")
  LISPOBJ(directory_absolute,"(:ABSOLUTE)") # Directory des leeren absoluten Pathname
 #endif
 #ifdef PATHNAME_RISCOS
  LISPOBJ(directory_absolute,"(:ABSOLUTE :ROOT)") # Directory des leeren absoluten Pathname
  LISPOBJ(directory_homedir,"(:ABSOLUTE :HOME)") # Directory des User-Homedir-Pathname
 #endif
 #ifdef USER_HOMEDIR
  LISPOBJ(user_homedir,"#\".\"") # User-Homedir-Pathname
 #endif
 #ifdef HAVE_SHELL
 #ifdef UNIX
  LISPOBJ(command_shell,"\""SHELL"\"") # Kommando-Shell als String
  LISPOBJ(command_shell_option,"\"-c\"") # Kommando-Shell-Option für Kommando
  LISPOBJ(user_shell,"\"/bin/csh\"") # User-Shell als String
 #endif
 #ifdef MSDOS
  LISPOBJ(command_shell,"\"\\\\COMMAND.COM\"") # Kommandointerpreter als String
  LISPOBJ(command_shell_option,"\"/C\"") # Kommandointerpreter-Option für Kommando
 #endif
 #ifdef WIN32_NATIVE
  LISPOBJ(command_shell,"NIL") # Kommandointerpreter als String
 #endif
 #ifdef RISCOS
  LISPOBJ(command_shell,"\"gos\"")
 #endif
 #endif
  # Liste aller offenen Channel-Streams, Terminal-Streams:
  LISPOBJ(open_files,"NIL")
 #ifdef GC_CLOSES_FILES
  # Während der GC: die Liste der nach der GC zu schließenden File-Streams:
  LISPOBJ(files_to_close,"NIL")
 #endif
  # Argumentliste für WRITE-TO-STRING :
  LISPOBJ(base10_radixnil,"(:BASE 10 :RADIX NIL)")
  # Defaults-Warnungs-String:
  LISPOBJ_LS(defaults_warn_string,
    /* ENGLISH */ "The value of ~S was not a pathname. ~:*~S is being reset.")
  # Defaultwert für :DIRECTORY-Argument:
  LISPOBJ(directory_default,"(:RELATIVE)")
  # Message-String:
  LISPOBJ_LS(mkdirp_string,
    /* ENGLISH */ "Creating directory: ")
  # Defaults für COMPILE-FILE-Aufruf in SPVW:
  LISPOBJ(source_file_type,"#\".lsp\"")
  LISPOBJ(compiled_file_type,"#\".fas\"")
  LISPOBJ(listing_file_type,"#\".lis\"")
# zu STREAM.D:
  #if defined(SPVW_PURE) || ((((STACK_ADDRESS_RANGE << addr_shift) >> garcol_bit_o) & 1) != 0)
  LISPOBJ(dynamic_bit_vector,"NIL") # Cache for macro DYNAMIC_BIT_VECTOR
  LISPOBJ(dynamic_string,"NIL") # Cache for macro DYNAMIC_STRING
  #endif
  LISPOBJ(class_fundamental_stream,"NIL") # #<STANDARD-CLASS FUNDAMENTAL-STREAM>
  LISPOBJ(class_fundamental_input_stream,"NIL") # #<STANDARD-CLASS FUNDAMENTAL-INPUT-STREAM>
  LISPOBJ(class_fundamental_output_stream,"NIL") # #<STANDARD-CLASS FUNDAMENTAL-OUTPUT-STREAM>
  LISPOBJ(type_input_stream,"(SATISFIES INPUT-STREAM-P)") # Typ für Fehlermeldung
  LISPOBJ(type_output_stream,"(SATISFIES OUTPUT-STREAM-P)") # Typ für Fehlermeldung
  LISPOBJ(type_string_with_fill_pointer,"(AND STRING (SATISFIES ARRAY-HAS-FILL-POINTER-P))") # Typ für Fehlermeldung
  LISPOBJ(setf_stream_element_type,"(SETF STREAM-ELEMENT-TYPE)")
  LISPOBJ(type_endianness,"(MEMBER :LITTLE :BIG)") # Typ für Fehlermeldung
  LISPOBJ(type_open_file_stream,"(AND FILE-STREAM (SATISFIES OPEN-STREAM-P))") # Typ für Fehlermeldung
  LISPOBJ(strmtype_ubyte8,"(UNSIGNED-BYTE 8)") # als Stream-Element-Type
# zu IO.D:
  # 4 Readtable-Case-Werte:
  LISPOBJ(rtcase_0,":UPCASE")
  LISPOBJ(rtcase_1,":DOWNCASE")
  LISPOBJ(rtcase_2,":PRESERVE")
  LISPOBJ(rtcase_3,":INVERT")
 # zum Reader:
  # Standard-Readtable von Common Lisp
  LISPOBJ(standard_readtable,".")
  # Prototyp der Dispatch-Reader-Funktionen
  LISPOBJ(dispatch_reader,"NIL")
  LISPOBJ(dispatch_reader_index,"0")
  # Präfix für Character-Namen:
  LISPOBJ(charname_prefix,"\"Code\"")
  # interne Variablen des Readers:
  LISPOBJ(token_buff_1,".")
  LISPOBJ(token_buff_2,".")
  LISPOBJ(displaced_string,".")
  # Handler-Typen:
  LISPOBJ(handler_for_arithmetic_error,"(#(ARITHMETIC-ERROR NIL))")
  LISPOBJ_S(tildeA,"~A")
 # zum Printer:
  # beim Ausgeben von Objekten verwendete Teilstrings:
  LISPOBJ_S(printstring_array,"ARRAY")
  LISPOBJ_S(printstring_fill_pointer,"FILL-POINTER=")
  LISPOBJ_S(printstring_address,"ADDRESS")
  LISPOBJ_S(printstring_system,"SYSTEM-POINTER")
  LISPOBJ_S(printstring_frame_pointer,"FRAME-POINTER")
  LISPOBJ_S(printstring_read_label,"READ-LABEL")
  LISPOBJ_S(printstring_unbound,"#<UNBOUND>")
  LISPOBJ_S(printstring_special_reference,"#<SPECIAL REFERENCE>")
  LISPOBJ_S(printstring_disabled_pointer,"#<DISABLED POINTER>")
  LISPOBJ_S(printstring_dot,"#<DOT>")
  LISPOBJ_S(printstring_eof,"#<END OF FILE>")
  LISPOBJ_S(printstring_hash_table,"HASH-TABLE")
  LISPOBJ_S(printstring_deleted,"DELETED ")
  LISPOBJ_S(printstring_package,"PACKAGE")
  LISPOBJ_S(printstring_readtable,"READTABLE")
  LISPOBJ(pathname_slotlist,"#.(list (cons :HOST #'pathname-host) (cons :DEVICE #'pathname-device) (cons :DIRECTORY #'pathname-directory) (cons :NAME #'pathname-name) (cons :TYPE #'pathname-type) (cons :VERSION #'pathname-version))")
  LISPOBJ(byte_slotlist,"#.(list (cons :SIZE #'byte-size) (cons :POSITION #'byte-position))")
  LISPOBJ_S(printstring_symbolmacro,"SYMBOL-MACRO")
  LISPOBJ_S(printstring_encoding,"ENCODING")
  #ifdef FOREIGN
  LISPOBJ_S(printstring_invalid,"INVALID ")
  LISPOBJ_S(printstring_fpointer,"FOREIGN-POINTER")
  #endif
  #ifdef DYNAMIC_FFI
  LISPOBJ_S(printstring_faddress,"FOREIGN-ADDRESS")
  LISPOBJ_S(printstring_fvariable,"FOREIGN-VARIABLE")
  LISPOBJ_S(printstring_ffunction,"FOREIGN-FUNCTION")
  #endif
  LISPOBJ_S(printstring_weakpointer,"WEAK-POINTER")
  LISPOBJ_S(printstring_broken_weakpointer,"#<BROKEN WEAK-POINTER>")
  LISPOBJ_S(printstring_finalizer,"#<FINALIZER>")
  #ifdef SOCKET_STREAMS
  LISPOBJ_S(printstring_socket_server,"SOCKET-SERVER")
  #endif
  #ifdef YET_ANOTHER_RECORD
  LISPOBJ_S(printstring_yetanother,"YET-ANOTHER")
  #endif
  LISPOBJ_S(printstring_closure,"CLOSURE")
  LISPOBJ_S(printstring_generic_function,"GENERIC-FUNCTION")
  LISPOBJ_S(printstring_compiled_closure,"COMPILED-CLOSURE")
  LISPOBJ_S(printstring_subr,"SYSTEM-FUNCTION")
  LISPOBJ_S(printstring_addon_subr,"ADD-ON-SYSTEM-FUNCTION")
  LISPOBJ_S(printstring_fsubr,"SPECIAL-OPERATOR")
  LISPOBJ_S(printstring_closed,"CLOSED ")
  LISPOBJ_S(printstring_buffered,"BUFFERED ")
  LISPOBJ_S(printstring_unbuffered,"UNBUFFERED ")
    # Namensstring zu jedem Streamtyp, adressiert durch Streamtyp:
    LISPOBJ_S(printstring_strmtype_synonym,"SYNONYM")
    LISPOBJ_S(printstring_strmtype_broad,"BROADCAST")
    LISPOBJ_S(printstring_strmtype_concat,"CONCATENATED")
    LISPOBJ_S(printstring_strmtype_twoway,"TWO-WAY")
    LISPOBJ_S(printstring_strmtype_echo,"ECHO")
    LISPOBJ_S(printstring_strmtype_str_in,"STRING-INPUT")
    LISPOBJ_S(printstring_strmtype_str_out,"STRING-OUTPUT")
    LISPOBJ_S(printstring_strmtype_str_push,"STRING-PUSH")
    LISPOBJ_S(printstring_strmtype_pphelp,"PRETTY-PRINTER-HELP")
    LISPOBJ_S(printstring_strmtype_buff_in,"BUFFERED-INPUT")
    LISPOBJ_S(printstring_strmtype_buff_out,"BUFFERED-OUTPUT")
    #ifdef GENERIC_STREAMS
    LISPOBJ_S(printstring_strmtype_generic,"GENERIC")
    #endif
    LISPOBJ_S(printstring_strmtype_file,"FILE")
    #ifdef KEYBOARD
    LISPOBJ_S(printstring_strmtype_keyboard,"KEYBOARD")
    #endif
    LISPOBJ_S(printstring_strmtype_terminal,"TERMINAL")
    #ifdef SCREEN
    LISPOBJ_S(printstring_strmtype_window,"WINDOW")
    #endif
    #ifdef PRINTER
    LISPOBJ_S(printstring_strmtype_printer,"PRINTER")
    #endif
    #ifdef PIPES
    LISPOBJ_S(printstring_strmtype_pipe_in,"PIPE-INPUT")
    LISPOBJ_S(printstring_strmtype_pipe_out,"PIPE-OUTPUT")
    #endif
    #ifdef X11SOCKETS
    LISPOBJ_S(printstring_strmtype_x11socket,"X11-SOCKET")
    #endif
    #ifdef SOCKET_STREAMS
    LISPOBJ_S(printstring_strmtype_socket,"SOCKET")
    LISPOBJ_S(printstring_strmtype_twoway_socket,"SOCKET")
    #endif
  LISPOBJ_S(printstring_stream,"-STREAM")
# zu LISPARIT.D:
  # verschiedene konstante Zahlen:
  #ifndef WIDE
  LISPOBJ(FF_zero,"0.0F0")
  LISPOBJ(FF_one,"1.0F0")
  LISPOBJ(FF_minusone,"-1.0F0")
  #endif
  LISPOBJ(DF_zero,"0.0D0")
  LISPOBJ(DF_one,"1.0D0")
  LISPOBJ(DF_minusone,"-1.0D0")
  # Defaultlänge beim Einlesen von Long-Floats (Integer >=LF_minlen, <2^intWCsize):
  LISPOBJ(LF_digits,".") # (schon initialisiert)
  # variable Long-Floats: (schon initialisiert)
  LISPOBJ(SF_pi,".")   # Wert von pi als Short-Float
  LISPOBJ(FF_pi,".")   # Wert von pi als Single-Float
  LISPOBJ(DF_pi,".")   # Wert von pi als Double-Float
  LISPOBJ(pi,".")      # Wert von pi, Long-Float der Defaultlänge
  LISPOBJ(LF_pi,".")   # Wert von pi, so genau wie bekannt
  LISPOBJ(LF_ln2,".")  # Wert von ln 2, so genau wie bekannt
  LISPOBJ(LF_ln10,".") # Wert von ln 10, so genau wie bekannt
  # Warnungs-Strings:
  LISPOBJ_LS(default_float_format_warnung_string,
    /* ENGLISH */ "The variable ~S had an illegal value." NLstring "~S has been reset to ~S.")
  LISPOBJ_LS(fpcontagion_warn_string,
     /* ENGLISH */ "Floating point operation combines numbers of different precision." NLstring "See ANSI CL 12.1.4.4 and the CLISP impnotes for details." NLstring "The result's actual precision is controlled by" NLstring "~S." NLstring "To shut off this warning, set ~S to ~S.")
# zu EVAL.D:
  # Toplevel-Deklarations-Environment:
  LISPOBJ(top_decl_env,"(NIL)") # Liste aus O(declaration_types) (wird nachinitialisiert)
  # Decl-Spec mit Liste der zu erkennenden Deklarations-Typen:
  LISPOBJ(declaration_types,"(DECLARATION OPTIMIZE DECLARATION)")
  # Name der Common-Lisp-Package:
  LISPOBJ_S(common_lisp_string,"COMMON-LISP")
# zu DEBUG.D:
  LISPOBJ_S(newline_string,NLstring)
  # Prompts:
  LISPOBJ_S(prompt_string,"> ")
  LISPOBJ_S(breakprompt_string,". Break> ")
  # Abschieds-String:
  LISPOBJ_LS(bye_string,
    /* ENGLISH */ "Bye.")
  LISPOBJ_LS(keypress_string,
    /* ENGLISH */ "Press a key to terminate...")
  # verschiedene Strings zur Beschreibung des Stacks:
  LISPOBJ_S(showstack_string_lisp_obj,NLstring "- ")
  LISPOBJ_S(showstack_string_bindung,NLstring "  | ")
  LISPOBJ_LS(showstack_string_next_env,
    /* ENGLISH */ NLstring "  Next environment: ")
  LISPOBJ_LS(showstack_string_TRAPPED_APPLY_frame,
    /* ENGLISH */ NLstring "APPLY frame with breakpoint for call ")
  LISPOBJ_LS(showstack_string_APPLY_frame,
    /* ENGLISH */ NLstring "APPLY frame for call ")
  LISPOBJ_LS(showstack_string_TRAPPED_EVAL_frame,
    /* ENGLISH */ NLstring "EVAL frame with breakpoint for form ")
  LISPOBJ_LS(showstack_string_EVAL_frame,
    /* ENGLISH */ NLstring "EVAL frame for form ")
  LISPOBJ_LS(showstack_string_DYNBIND_frame,
    /* ENGLISH */ NLstring "frame binding variables (~ = dynamically):")
  #ifdef HAVE_SAVED_REGISTERS
  LISPOBJ_LS(showstack_string_CALLBACK_frame,
    /* ENGLISH */ NLstring "CALLBACK frame")
  #endif
  LISPOBJ_LS(showstack_string_VAR_frame,
    /* ENGLISH */ NLstring "frame binding variables ")
  LISPOBJ_LS(showstack_string_FUN_frame,
    /* ENGLISH */ NLstring "frame binding functions ")
  LISPOBJ_LS(showstack_string_binds,
    /* ENGLISH */ " binds (~ = dynamically):")
  LISPOBJ_S(showstack_string_zuord," <--> ")
  LISPOBJ_LS(showstack_string_IBLOCK_frame,
    /* ENGLISH */ NLstring "block frame ")
  LISPOBJ_LS(showstack_string_NESTED_IBLOCK_frame,
    /* ENGLISH */ NLstring "nested block frame ")
  LISPOBJ_LS(showstack_string_for1,
    /* ENGLISH */ " for ")
  LISPOBJ_LS(showstack_string_CBLOCK_frame,
    /* ENGLISH */ NLstring "compiled block frame for ")
  LISPOBJ_LS(showstack_string_ITAGBODY_frame,
    /* ENGLISH */ NLstring "tagbody frame ")
  LISPOBJ_LS(showstack_string_NESTED_ITAGBODY_frame,
    /* ENGLISH */ NLstring "nested tagbody frame ")
  LISPOBJ_LS(showstack_string_for2,
    /* ENGLISH */ " for")
  LISPOBJ_S(showstack_string_zuordtag," --> ")
  LISPOBJ_LS(showstack_string_CTAGBODY_frame,
    /* ENGLISH */ NLstring "compiled tagbody frame for ")
  LISPOBJ_LS(showstack_string_CATCH_frame,
    /* ENGLISH */ NLstring "catch frame for tag ")
  LISPOBJ_LS(showstack_string_HANDLER_frame,
    /* ENGLISH */ NLstring "handler frame for conditions")
  LISPOBJ_LS(showstack_string_UNWIND_PROTECT_frame,
    /* ENGLISH */ NLstring "unwind-protect frame")
  LISPOBJ_LS(showstack_string_DRIVER_frame,
    /* ENGLISH */ NLstring NLstring "driver frame")
  LISPOBJ_LS(showstack_string_ENV_frame,
    /* ENGLISH */ NLstring "frame binding environments")
  LISPOBJ_S(showstack_string_VENV_frame,NLstring "  VAR_ENV <--> ")
  LISPOBJ_S(showstack_string_FENV_frame,NLstring "  FUN_ENV <--> ")
  LISPOBJ_S(showstack_string_BENV_frame,NLstring "  BLOCK_ENV <--> ")
  LISPOBJ_S(showstack_string_GENV_frame,NLstring "  GO_ENV <--> ")
  LISPOBJ_S(showstack_string_DENV_frame,NLstring "  DECL_ENV <--> ")
# zu REXX.D:
 #ifdef REXX
  LISPOBJ(rexx_inmsg_list,"NIL")
  LISPOBJ(rexx_prefetch_inmsg,"NIL")
  LISPOBJ(type_rexx_host,"(OR STRING BOOLEAN)")
 #endif
# zu FOREIGN.D:
 #ifdef DYNAMIC_FFI
  LISPOBJ(fp_zero,"NIL")
  LISPOBJ(foreign_variable_table,"#.(make-hash-table :test #'equal)")
  LISPOBJ(foreign_function_table,"#.(make-hash-table :test #'equal)")
  #ifdef AMIGAOS
  LISPOBJ(foreign_libraries,"NIL")
  #endif
  LISPOBJ(foreign_callin_table,"#.(make-hash-table :test #'eq)")
  LISPOBJ(foreign_callin_vector,"#.(let ((array (make-array 1 :adjustable t :fill-pointer 1))) (sys::store array 0 0) array)")
 #endif
