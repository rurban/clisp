# Evaluator, Applyer und Bytecode-Interpreter für CLISP
# Bruno Haible 1990-1999

#include "lispbibl.c"


# Funktionen-Tabelle:
# Darin stehen nur SUBRs, die der Compiler "inline" machen darf.
# In FUNTAB1 und FUNTAB2 stehen SUBRs ohne Rest-Parameter (also
# mit zur Compile-Zeit bekannter fester Argumentezahl).
# In FUNTABR stehen SUBRs mit Rest-Parameter.
  #define _(name)  &subr_tab.D_##name  # Adresse von SUBR name, wie L(name)
  # erst FUNTAB1 und FUNTAB2 :
  local const Subr FUNTAB[] = {
    # SPVW : 0 SUBRs
    # EVAL : 2 SUBRs
    _(funtabref), _(subr_info),
    # ARRAY : 30-2 SUBRs
    _(copy_simple_vector), /* _(svref), _(psvstore), */ _(row_major_aref),
    _(row_major_store), _(array_element_type), _(array_rank),
    _(array_dimension), _(array_dimensions), _(array_total_size),
    _(adjustable_array_p), _(bit_and), _(bit_ior), _(bit_xor), _(bit_eqv),
    _(bit_nand), _(bit_nor), _(bit_andc1), _(bit_andc2), _(bit_orc1),
    _(bit_orc2), _(bit_not), _(array_has_fill_pointer_p), _(fill_pointer),
    _(set_fill_pointer), _(vector_push), _(vector_pop), _(vector_push_extend),
    _(make_array), _(adjust_array),
    # CHARSTRG : 47 SUBRs
    _(standard_char_p), _(graphic_char_p), _(string_char_p), _(alpha_char_p),
    _(upper_case_p), _(lower_case_p), _(both_case_p), _(digit_char_p),
    _(alphanumericp), _(char_code), _(code_char), _(character), _(char_upcase),
    _(char_downcase), _(digit_char), _(char_int), _(int_char), _(char_name),
    _(char), _(schar), _(store_char), _(store_schar), _(string_gleich),
    _(string_ungleich), _(string_kleiner), _(string_groesser),
    _(string_klgleich), _(string_grgleich), _(string_equal),
    _(string_not_equal), _(string_lessp), _(string_greaterp),
    _(string_not_greaterp), _(string_not_lessp), _(search_string_gleich),
    _(search_string_equal), _(make_string), _(string_both_trim),
    _(nstring_upcase), _(string_upcase), _(nstring_downcase),
    _(string_downcase), _(nstring_capitalize), _(string_capitalize),
    _(string), _(name_char), _(substring),
    # CONTROL : 21-2 SUBRs
    _(symbol_value), /* _(symbol_function), */ _(boundp), _(fboundp),
    _(special_operator_p), _(set), _(makunbound), _(fmakunbound), /* _(values_list), */
    _(driver), _(unwind_to_driver), _(old_macro_function), _(macroexpand),
    _(macroexpand_1), _(proclaim), _(eval), _(evalhook), _(applyhook),
    _(constantp), _(parse_body), _(keyword_test),
    # DEBUG : 0 SUBRs
    # ERROR : 1 SUBR
    _(invoke_debugger),
    # HASHTABL : 11 SUBRs
    _(make_hash_table), _(gethash), _(puthash), _(remhash), _(maphash),
    _(clrhash), _(hash_table_count), _(hash_table_iterator),
    _(hash_table_iterate), _(class_gethash), _(sxhash),
    # IO : 36 SUBRs
    _(copy_readtable), _(set_syntax_from_char), _(set_macro_character),
    _(get_macro_character), _(make_dispatch_macro_character),
    _(set_dispatch_macro_character), _(get_dispatch_macro_character),
    _(read), _(read_preserving_whitespace), _(read_delimited_list),
    _(read_line), _(read_char), _(unread_char), _(peek_char), _(listen),
    _(read_char_no_hang), _(clear_input), _(read_from_string),
    _(parse_integer), _(write), _(prin1), _(print), _(pprint), _(princ),
    _(write_to_string), _(prin1_to_string), _(princ_to_string), _(write_char),
    _(write_string), _(write_line), _(terpri), _(fresh_line),
    _(finish_output), _(force_output), _(clear_output), _(line_position),
    # LIST : 83-36 SUBRs
    /* _(car), _(cdr), _(caar), _(cadr), _(cdar), _(cddr), _(caaar), _(caadr),
    _(cadar), _(caddr), _(cdaar), _(cdadr), _(cddar), _(cdddr), _(caaaar),
    _(caaadr), _(caadar), _(caaddr), _(cadaar), _(cadadr), _(caddar),
    _(cadddr), _(cdaaar), _(cdaadr), _(cdadar), _(cdaddr), _(cddaar),
    _(cddadr), _(cdddar), _(cddddr), _(cons), */ _(tree_equal), _(endp),
    _(list_length), _(nth), /* _(first), _(second), _(third), _(fourth), */
    _(fifth), _(sixth), _(seventh), _(eighth), _(ninth), _(tenth), /* _(rest), */
    _(nthcdr), _(last), _(make_list), _(copy_list), _(copy_alist),
    _(copy_tree), _(revappend), _(nreconc), _(list_nreverse), _(butlast),
    _(nbutlast), _(ldiff), _(rplaca), _(prplaca), _(rplacd), _(prplacd),
    _(subst), _(subst_if), _(subst_if_not), _(nsubst), _(nsubst_if),
    _(nsubst_if_not), _(sublis), _(nsublis), _(member), _(member_if),
    _(member_if_not), _(tailp), _(adjoin), _(acons), _(pairlis), _(assoc),
    _(assoc_if), _(assoc_if_not), _(rassoc), _(rassoc_if), _(rassoc_if_not),
    # MISC : 10 SUBRs
    _(lisp_implementation_type), _(lisp_implementation_version),
    _(software_type), _(software_version), _(identity), _(get_universal_time),
    _(get_internal_run_time), _(get_internal_real_time), _(sleep), _(time),
    # PACKAGE : 26 SUBRs
    _(make_symbol), _(find_package), _(package_name), _(package_nicknames),
    _(rename_package), _(package_use_list), _(package_used_by_list),
    _(package_shadowing_symbols), _(list_all_packages), _(intern),
    _(find_symbol), _(unintern), _(export), _(unexport), _(import),
    _(shadowing_import), _(shadow), _(use_package), _(unuse_package),
    _(make_package), _(pin_package), _(in_package), _(find_all_symbols),
    _(map_symbols), _(map_external_symbols), _(map_all_symbols),
    # PATHNAME : 27 SUBRs
    _(parse_namestring), _(pathname), _(pathnamehost), _(pathnamedevice),
    _(pathnamedirectory), _(pathnamename), _(pathnametype),
    _(pathnameversion), _(file_namestring), _(directory_namestring),
    _(host_namestring), _(merge_pathnames), _(enough_namestring),
    _(make_pathname), _(namestring), _(truename), _(probe_file),
    _(delete_file), _(rename_file), _(old_open), _(directory), _(cd),
    _(make_dir), _(delete_dir), _(file_write_date), _(file_author),
    _(savemem),
    # PREDTYPE : 46-3 SUBRs
    /* _(eq), */ _(eql), _(equal), _(equalp), _(consp), _(atom), _(symbolp),
    _(stringp), _(numberp), _(compiled_function_p), /* _(null), _(not), */
    _(closurep), _(listp), _(integerp), _(fixnump), _(rationalp), _(floatp),
    _(short_float_p), _(single_float_p), _(double_float_p), _(long_float_p),
    _(realp), _(complexp), _(streamp), _(random_state_p), _(readtablep),
    _(hash_table_p), _(pathnamep), _(logical_pathname_p), _(characterp),
    _(functionp), _(generic_function_p), _(packagep), _(arrayp),
    _(simple_array_p), _(bit_vector_p), _(vectorp), _(simple_vector_p),
    _(simple_string_p), _(simple_bit_vector_p), _(commonp), _(type_of),
    _(class_of), _(find_class), _(coerce),
    # RECORD : 22 SUBRs
    _(record_ref), _(record_store), _(record_length), _(structure_ref),
    _(structure_store), _(make_structure), _(copy_structure),
    _(structure_type_p), _(closure_name), _(closure_codevec),
    _(closure_consts), _(make_code_vector), _(make_closure),
    _(make_load_time_eval), _(structure_object_p), _(std_instance_p),
    _(old_pallocate_instance), _(slot_value), _(set_slot_value), _(slot_boundp),
    _(slot_makunbound), _(slot_exists_p),
    # SEQUENCE : 40 SUBRs
    _(sequencep), _(elt), _(setelt), _(subseq), _(copy_seq), _(length),
    _(reverse), _(nreverse), _(make_sequence), _(reduce), _(fill),
    _(replace), _(remove), _(remove_if), _(remove_if_not), _(delete),
    _(delete_if), _(delete_if_not), _(remove_duplicates),
    _(delete_duplicates), _(substitute), _(substitute_if),
    _(substitute_if_not), _(nsubstitute), _(nsubstitute_if),
    _(nsubstitute_if_not), _(find), _(find_if), _(find_if_not), _(position),
    _(position_if), _(position_if_not), _(count), _(count_if),
    _(count_if_not), _(mismatch), _(search), _(sort), _(stable_sort),
    _(merge),
    # STREAM : 24 SUBRs
    _(file_stream_p), _(make_synonym_stream), _(synonym_stream_p),
    _(broadcast_stream_p), _(concatenated_stream_p), _(make_two_way_stream),
    _(two_way_stream_p), _(make_echo_stream), _(echo_stream_p),
    _(make_string_input_stream), _(string_input_stream_index),
    _(make_string_output_stream), _(get_output_stream_string),
    _(make_string_push_stream), _(string_stream_p), _(input_stream_p),
    _(output_stream_p), _(built_in_stream_element_type),
    _(stream_external_format), _(built_in_stream_close), _(read_byte),
    _(write_byte), _(file_position), _(file_length),
    # SYMBOL : 15 SUBRs
    _(putd), _(proclaim_constant), _(get), _(getf), _(get_properties),
    _(putplist), _(put), _(remprop), _(symbol_package), _(symbol_plist),
    _(symbol_name), _(keywordp), _(gensym), _(special_variable_p), _(gensym),
    # LISPARIT : 84 SUBRs
    _(decimal_string), _(zerop), _(plusp), _(minusp), _(oddp), _(evenp),
    _(einsplus), _(einsminus), _(conjugate), _(exp), _(expt), _(log),
    _(sqrt), _(isqrt), _(abs), _(phase), _(signum), _(sin), _(cos), _(tan),
    _(cis), _(asin), _(acos), _(atan), _(sinh), _(cosh), _(tanh), _(asinh),
    _(acosh), _(atanh), _(float), _(rational), _(rationalize), _(numerator),
    _(denominator), _(floor), _(ceiling), _(truncate), _(round), _(mod),
    _(rem), _(ffloor), _(fceiling), _(ftruncate), _(fround), _(decode_float),
    _(scale_float), _(float_radix), _(float_sign), _(float_digits),
    _(float_precision), _(integer_decode_float), _(complex), _(realpart),
    _(imagpart), _(lognand), _(lognor), _(logandc1), _(logandc2), _(logorc1),
    _(logorc2), _(boole), _(lognot), _(logtest), _(logbitp), _(ash),
    _(logcount), _(integer_length), _(byte), _(bytesize), _(byteposition),
    _(ldb), _(ldb_test), _(mask_field), _(dpb), _(deposit_field), _(random),
    _(make_random_state), _(fakultaet), _(exquo), _(long_float_digits),
    _(set_long_float_digits), _(log2), _(log10),
    # sonstige:
    _(copy_generic_function),
    };
  # Das waren 525-43+1 SUBRs.
  # Nun FUNTABR :
  local const Subr FUNTABR[] = {
    # SPVW : 0 SUBRs
    # EVAL : 0 SUBRs
    # ARRAY : 7 SUBRs
    _(vector), _(aref), _(store), _(array_in_bounds_p),
    _(array_row_major_index), _(bit), _(sbit),
    # CHARSTRG : 13 SUBRs
    _(char_gleich), _(char_ungleich), _(char_kleiner), _(char_groesser),
    _(char_klgleich), _(char_grgleich), _(char_equal), _(char_not_equal),
    _(char_lessp), _(char_greaterp), _(char_not_greaterp), _(char_not_lessp),
    _(string_concat),
    # CONTROL : 10 SUBRs
    _(apply), _(pfuncall), _(funcall), _(mapcar), _(maplist), _(mapc),
    _(mapl), _(mapcan), _(mapcon), _(values),
    # DEBUG : 0 SUBRs
    # ERROR : 2 SUBRs
    _(error), _(error_of_type),
    # HASHTABL : 1 SUBR
    _(class_tuple_gethash),
    # IO : 0 SUBRs
    # LIST : 4 SUBRs
    _(list), _(liststern), _(append), _(nconc),
    # MISC : 0 SUBRs
    # PACKAGE : 0 SUBRs
    # PATHNAME : 0 SUBRs
    # PREDTYPE : 0 SUBRs
    # RECORD : 0 SUBRs
    # SEQUENCE : 6 SUBRs
    _(concatenate), _(map), _(some), _(every), _(notany), _(notevery),
    # STREAM : 2 SUBRs
    _(make_broadcast_stream), _(make_concatenated_stream),
    # SYMBOL : 0 SUBRs
    # LISPARIT : 18 SUBRs
    _(gleich), _(ungleich), _(kleiner), _(groesser), _(klgleich),
    _(grgleich), _(max), _(min), _(plus), _(minus), _(mal), _(durch), _(gcd),
    _(lcm), _(logior), _(logxor), _(logand), _(logeqv),
    };
  # Das waren 63 SUBRs.
  #undef _
  #define FUNTAB1  (&FUNTAB[0])
  #define FUNTAB2  (&FUNTAB[256])
  #define FUNTAB_length  (sizeof(FUNTAB)/sizeof(Subr))
  #define FUNTABR_length  (sizeof(FUNTABR)/sizeof(Subr))

# Argumenttyp-Kürzel bei compilierten Closures:
  typedef enum {cclos_argtype_default,
                cclos_argtype_0_0,
                cclos_argtype_1_0,
                cclos_argtype_2_0,
                cclos_argtype_3_0,
                cclos_argtype_4_0,
                cclos_argtype_5_0,
                cclos_argtype_0_1,
                cclos_argtype_1_1,
                cclos_argtype_2_1,
                cclos_argtype_3_1,
                cclos_argtype_4_1,
                cclos_argtype_0_2,
                cclos_argtype_1_2,
                cclos_argtype_2_2,
                cclos_argtype_3_2,
                cclos_argtype_0_3,
                cclos_argtype_1_3,
                cclos_argtype_2_3,
                cclos_argtype_0_4,
                cclos_argtype_1_4,
                cclos_argtype_0_5,
                cclos_argtype_0_0_rest,
                cclos_argtype_1_0_rest,
                cclos_argtype_2_0_rest,
                cclos_argtype_3_0_rest,
                cclos_argtype_4_0_rest,
                cclos_argtype_0_0_key,
                cclos_argtype_1_0_key,
                cclos_argtype_2_0_key,
                cclos_argtype_3_0_key,
                cclos_argtype_4_0_key,
                cclos_argtype_0_1_key,
                cclos_argtype_1_1_key,
                cclos_argtype_2_1_key,
                cclos_argtype_3_1_key,
                cclos_argtype_0_2_key,
                cclos_argtype_1_2_key,
                cclos_argtype_2_2_key,
                cclos_argtype_0_3_key,
                cclos_argtype_1_3_key,
                cclos_argtype_0_4_key,
                cclos_argtype_for_broken_compilers_that_dont_like_trailing_commas
               }
          cclos_argtype_;

# Aufruf des Bytecode-Interpreters:
# Interpretiert den Bytecode einer compilierten Closure.
# interpret_bytecode(closure,codevec,index);
# > closure: compilierte Closure
# > codevec: ihr Codevektor, ein Simple-Bit-Vector
# > index: Start-Index
# < mv_count/mv_space: Werte
# verändert STACK, kann GC auslösen
  # local Values interpret_bytecode (object closure, object codevec, uintL index);
  local Values interpret_bytecode_ (object closure, Sbvector codeptr, const uintB* byteptr);
  #define interpret_bytecode(closure,codevec,index)  \
    interpret_bytecode_(closure,TheSbvector(codevec),&TheSbvector(codevec)->data[index])

# GCC2 kann direkt zu Labels springen. Das gibt schnelleren Code als switch().
  #ifdef GNU
    #if (__GNUC__ >= 2) && !defined(UNIX_HPUX) && !defined(NO_FAST_DISPATCH) # HP-UX Linker Bug umgehen
      #define FAST_DISPATCH
      #if (__GNUC_MINOR__ >= 7) # gcc-2.6.3 Bug umgehen (-fno-defer-pop ginge auch)
        #define FAST_DISPATCH_THREADED
      #endif
    #endif
  #endif

# Werte der Bytecodes (256 Stück):
  #ifndef FAST_DISPATCH
    typedef enum {
                   #define BYTECODE(code)  code,
                   #include "bytecode.c"
                   #undef BYTECODE
                   cod_for_broken_compilers_that_dont_like_trailing_commas
                 }
            bytecode_enum;
  #endif


#        ---------------------- LISP-FUNKTIONEN -----------------------

# (SYS::%FUNTABREF i) liefert den Namen der Funktion Nr. i aus der Funktionen-
# tabelle (ein Symbol), bzw. NIL falls i nicht im richtigen Bereich liegt.
LISPFUNN(funtabref,1)
  { var object arg = popSTACK(); # Argument
    var uintL i;
    if (posfixnump(arg) # sollte ein Fixnum >=0
        && (i = posfixnum_to_L(arg),
            i < FUNTAB_length+FUNTABR_length # und < Tabellenlänge sein
       )   )
      # Name des indizierten Elements der Tabelle:
      { value1 = (i < FUNTAB_length
                  ? FUNTAB[i]                # aus FUNTAB1/2
                  : FUNTABR[i-FUNTAB_length] # bzw. aus FUNTABR
                 )->name;
      }
      else
      { value1 = NIL; } # oder NIL
    mv_count=1; # als Wert
  }

# (SYS::SUBR-INFO obj) liefert, wenn obj ein SUBR (oder ein Symbol mit einem
# SUBR als globaler Funktionsdefinition) ist, Information zu diesem SUBR,
# 6 Werte:
#   name              Name,
#   req-anz           Anzahl der required-Parameter,
#   opt-anz           Anzahl der optionalen Parameter,
#   rest-p            Flag, ob &rest angegeben,
#   keywords          Liste der zulässigen Keywords (leer: kein &key angegeben),
#   allow-other-keys  Flag, ob zusätzliche Keywords erlaubt sind,
# und sonst NIL.
LISPFUNN(subr_info,1)
  { var object obj = popSTACK();
    if (!subrp(obj))
      { if (!(symbolp(obj) && subrp(Symbol_function(obj))))
          { value1 = NIL; mv_count=0; return; } # kein SUBR -> kein Wert
        obj = Symbol_function(obj);
      }
    # obj ist ein SUBR
    pushSTACK(TheSubr(obj)->name); # Name
    pushSTACK(fixnum(TheSubr(obj)->req_anz)); # req-anz
    pushSTACK(fixnum(TheSubr(obj)->opt_anz)); # opt-anz
    pushSTACK(TheSubr(obj)->rest_flag == subr_norest ? NIL : T); # rest-p
    coerce_sequence(TheSubr(obj)->keywords,S(list));
    pushSTACK(value1); # Keyword-Vektor als Liste
    pushSTACK(TheSubr(obj)->key_flag == subr_key_allow ? T : NIL); # allow-other-keys
    funcall(L(values),6); # 6 Werte
  }


#        ----------------------- UNTERPROGRAMME -----------------------

# UP: Löst einen Frame auf, auf den STACK zeigt.
# unwind();
# Die Werte mv_count/mv_space bleiben dieselben.
# Falls es kein Unwind-Protect-Frame ist: kehrt normal zurück.
# Falls es ein Unwind-Protect-Frame ist:
#   rettet die Werte, klettert STACK und SP hoch
#   und springt dann unwind_protect_to_save.fun an.
# verändert STACK
# can trigger GC
  global void unwind (void);
  global void unwind()
    { var fcint frame_info = framecode(STACK_0);
      #ifdef unwind_bit_t
      if (frame_info & bit(unwind_bit_t)) # überhaupt etwas zu tun?
      #else
      if (frame_info >= unwind_limit_t) # überhaupt etwas zu tun?
      #endif
        # (Nein bei APPLY, EVAL ungetrapped, CATCH, HANDLER,
        #  IBLOCK und ITAGBODY ungenestet)
        { if ((frame_info & bit(skip2_bit_t)) == 0) # ENV-Frame oder DYNBIND-Frame?
            #ifdef entrypoint_bit_t
            if (frame_info & bit(entrypoint_bit_t)) # BLOCK, TAGBODY, CATCH etc. ?
            #else
            if (frame_info < entrypoint_limit_t) # BLOCK, TAGBODY, CATCH etc. ?
            #endif
              # Frame mit Exitpoint liegt vor
              if (frame_info & bit(blockgo_bit_t)) # BLOCK oder TAGBODY?
                # BLOCK_FRAME oder TAGBODY_FRAME liegt vor
                if (frame_info & bit(cframe_bit_t)) # compilierter?
                  # CBLOCK_FRAME oder CTAGBODY_FRAME liegt vor
                  { # Im Cons (NAME/Tags . <Framepointer>)
                    Cdr(STACK_(frame_ctag)) = disabled; # Exit/Tags disablen
                  }
                  else
                  # IBLOCK_FRAME oder ITAGBODY_FRAME liegt vor, genestet
                  { # Im Cons (NAME/Tags . <Framepointer>)
                    # (erstes Paar der Aliste next_env)
                    Cdr(Car(STACK_(frame_next_env))) = disabled; # Exit/Tags disablen
                  }
                else
                # UNWIND_PROTECT_FRAME, DRIVER_FRAME oder getrappter APPLY/EVAL_FRAME liegt vor
                if (frame_info & bit(dynjump_bit_t))
                  # UNWIND_PROTECT_FRAME oder DRIVER_FRAME liegt vor
                  if (frame_info & bit(driver_bit_t))
                    # DRIVER_FRAME liegt vor
                    {}
                    else
                    # UNWIND_PROTECT_FRAME liegt vor
                    { enter_frame_at_STACK(); }
                  else
                  # getrappter APPLY/EVAL_FRAME liegt vor
                  { # Wie im Tracer:
                    var object values;
                    mv_to_list(); values = popSTACK(); # Werte in Liste packen
                    dynamic_bind(S(trace_values),values); # *TRACE-VALUES* binden
                    break_driver(T); # Break-Driver aufrufen
                    list_to_mv(Symbol_value(S(trace_values)), # wieder Werte bilden
                               fehler_mv_zuviel(framecode(STACK_(0+3))==TRAPPED_EVAL_frame_info
                                                ? S(eval)
                                                : S(apply)
                                               );
                              );
                    dynamic_unbind(); # Bindung auflösen
                  }
              else
              # VAR_FRAME oder FUN_FRAME liegt vor
              { var object* new_STACK = topofframe(STACK_0); # Pointer übern Frame
                if (frame_info & bit(fun_bit_t))
                  {} # bei Funktionen nichts weiter zu tun
                  else
                  # VAR_FRAME liegt vor, bindingptr läuft durch die Bindungen hoch
                  { var object* frame_end = STACKpointable(new_STACK);
                    var object* bindingptr = &STACK_(frame_bindings); # Beginn der Variablen-/Funktionsbindungen
                    until (bindingptr == frame_end)
                      { if (as_oint(*(bindingptr STACKop 0)) & wbit(dynam_bit_o))
                          if (as_oint(*(bindingptr STACKop 0)) & wbit(active_bit_o))
                            # Bindung statisch oder inaktiv -> nichts zu tun
                            # Bindung dynamisch und aktiv -> Wert zurückschreiben:
                            { TheSymbolflagged(*(bindingptr STACKop varframe_binding_sym))->symvalue =
                                *(bindingptr STACKop varframe_binding_value);
                            }
                        bindingptr skipSTACKop varframe_binding_size; # nächste Bindung
                  }   }
                # STACK neu setzen, dadurch Frame auflösen:
                setSTACK(STACK = new_STACK);
                goto fertig;
              }
            else
            # DYNBIND_FRAME oder CALLBACK_FRAME oder ENV_FRAME liegt vor
            if (frame_info & bit(envbind_bit_t))
              # ENV_FRAME liegt vor
              { var object* ptr = &STACK_1;
                switch (frame_info & envbind_case_mask_t)
                  { case (ENV1V_frame_info & envbind_case_mask_t): # 1 VAR_ENV
                      aktenv.var_env = *ptr; ptr skipSTACKop 1; break;
                    case (ENV1F_frame_info & envbind_case_mask_t): # 1 FUN_ENV
                      aktenv.fun_env = *ptr; ptr skipSTACKop 1; break;
                    case (ENV1B_frame_info & envbind_case_mask_t): # 1 BLOCK_ENV
                      aktenv.block_env = *ptr; ptr skipSTACKop 1; break;
                    case (ENV1G_frame_info & envbind_case_mask_t): # 1 GO_ENV
                      aktenv.go_env = *ptr; ptr skipSTACKop 1; break;
                    case (ENV1D_frame_info & envbind_case_mask_t): # 1 DECL_ENV
                      aktenv.decl_env = *ptr; ptr skipSTACKop 1; break;
                    case (ENV2VD_frame_info & envbind_case_mask_t): # 1 VAR_ENV und 1 DECL_ENV
                      aktenv.var_env = *ptr; ptr skipSTACKop 1;
                      aktenv.decl_env = *ptr; ptr skipSTACKop 1;
                      break;
                    case (ENV5_frame_info & envbind_case_mask_t): # alle 5 Environments
                      aktenv.var_env = *ptr; ptr skipSTACKop 1;
                      aktenv.fun_env = *ptr; ptr skipSTACKop 1;
                      aktenv.block_env = *ptr; ptr skipSTACKop 1;
                      aktenv.go_env = *ptr; ptr skipSTACKop 1;
                      aktenv.decl_env = *ptr; ptr skipSTACKop 1;
                      break;
                    default: NOTREACHED
              }   }
              else
              #ifdef HAVE_SAVED_REGISTERS
              if (frame_info & bit(callback_bit_t))
                # CALLBACK_FRAME liegt vor
                { var object* new_STACK = topofframe(STACK_0); # Pointer übern Frame
                  # callback_saved_registers neu setzen:
                  callback_saved_registers = (struct registers *)(aint)as_oint(STACK_1);
                  # STACK neu setzen, dadurch Frame auflösen:
                  setSTACK(STACK = new_STACK);
                  goto fertig;
                }
                else
              #endif
                # DYNBIND_FRAME liegt vor
                { var object* new_STACK = topofframe(STACK_0); # Pointer übern Frame
                  var object* frame_end = STACKpointable(new_STACK);
                  var object* bindingptr = &STACK_1; # Beginn der Bindungen
                  # bindingptr läuft durch die Bindungen hoch
                  until (bindingptr == frame_end)
                    { Symbol_value(*(bindingptr STACKop 0)) = *(bindingptr STACKop 1);
                      bindingptr skipSTACKop 2; # nächste Bindung
                    }
                  # STACK neu setzen, dadurch Frame auflösen:
                  setSTACK(STACK = new_STACK);
                  goto fertig;
                }
        }
      # STACK neu setzen, dadurch Frame auflösen:
      setSTACK(STACK = topofframe(STACK_0));
      fertig: ;
    }

# UP: "unwindet" den STACK bis zum nächsten DRIVER_FRAME und
# springt in die entsprechende Top-Level-Schleife.
# reset();
  nonreturning_function(global, reset, (void));
  global void reset()
    { # Beim Auflösen von UNWIND-PROTECT-Frames keine Werte retten:
      value1 = NIL; mv_count=0;
      unwind_protect_to_save.fun = (restart)&reset;
      loop
        { # Hört der STACK hier auf?
          if (eq(STACK_0,nullobj) && eq(STACK_1,nullobj))
            { driver(); quit(); } # STACK völlig weg -> Neustart
          if (framecode(STACK_0) & bit(frame_bit_t))
            # Bei STACK_0 beginnt ein Frame
            { if (framecode(STACK_0) == DRIVER_frame_info) # DRIVER_FRAME ?
                break; # ja -> gefunden
              unwind(); # Frame auflösen
            }
            else
            # STACK_0 enthält ein normales LISP-Objekt
            { skipSTACK(1); }
        }
      # Bei STACK_0 beginnt ein Driver-Frame.
      enter_frame_at_STACK();
    }

# UP: bindet dynamisch die Symbole der Liste symlist
# an die Werte aus der Liste vallist.
# progv(symlist,vallist);
# > symlist, vallist: zwei Listen
# Es wird genau ein Variablenbindungsframe aufgebaut.
# verändert STACK
  global void progv (object symlist, object vallist);
  global void progv(symlist,vallist)
    var object symlist;
    var object vallist;
    { # Platz auf dem STACK verlangen:
      get_space_on_STACK(llength(symlist)*2*sizeof(object));
      # Frame aufbauen:
      { var object* top_of_frame = STACK; # Pointer übern Frame
        var object symlistr = symlist;
        while (consp(symlistr)) # Symbolliste durchgehen
          { var object sym = Car(symlistr);
            if (!symbolp(sym)) { fehler_kein_symbol(S(progv),sym); }
            if (constantp(TheSymbol(sym)))
              { pushSTACK(sym);
                pushSTACK(S(progv));
                fehler(program_error,
                       GETTEXT("~: ~ is a constant, cannot be bound dynamically")
                      );
              }
            pushSTACK(Symbol_value(sym)); # alter Wert der Variablen
            pushSTACK(sym); # Variable
            symlistr = Cdr(symlistr);
          }
        finish_frame(DYNBIND);
        # Frame fertig aufgebaut, nun die Werte der Variablen verändern:
        while (consp(symlist))
          { if (atomp(vallist))
              # Wertliste kürzer als Symbolliste
              # -> alle weiteren "Werte" sind #<UNBOUND>
              { do { Symbol_value(Car(symlist)) = unbound;
                     symlist = Cdr(symlist);
                   }
                   while (consp(symlist));
                break;
              }
            # Symbol bekommt neuen Wert:
            Symbol_value(Car(symlist)) = Car(vallist);
            symlist = Cdr(symlist); vallist = Cdr(vallist);
          }
    } }

# UP: Löst die dynamische Schachtelung im STACK auf bis zu dem Frame
# (ausschließlich), auf den upto zeigt, und springt diesen dann an.
# unwind_upto(upto);
# > upto: Pointer auf einen Frame (in den Stack, ohne Typinfo).
# Rettet die Werte mv_count/mv_space.
# verändert STACK,SP
# can trigger GC
# Springt dann den gefundenen Frame an.
  nonreturning_function(global, unwind_upto, (object* upto_frame));
  global void unwind_upto(upto_frame)
    var object* upto_frame;
    { unwind_protect_to_save.fun        = &unwind_upto;
      unwind_protect_to_save.upto_frame = upto_frame;
      until (STACK == upto_frame) # am Ziel-Frame angelangt?
        { if (framecode(STACK_0) & bit(frame_bit_t)) # liegt ein Frame vor?
            { unwind(); } # ja -> auflösen
            # (Sollte dies ein Unwind-Protect-Frame sein, so wird danach wieder
            # unwind_upto(upto_frame) aufgerufen, und wir sind wieder hier.)
            else
            { skipSTACK(1); } # nein -> einfach weiter
        }
      # Nun zeigt STACK auf den gefundenen FRAME.
      enter_frame_at_STACK();
    }

# UP: throwt zum Tag tag und übergibt dabei die Werte mv_count/mv_space.
# Kommt nur dann zurück, wenn es keinen CATCH-Frame dieses Tags gibt.
# throw_to(tag);
  global void throw_to (object tag);
  global void throw_to(tag)
    var object tag;
    { # Suche nach Catch-Frame mit Tag =tag:
      var object* FRAME = STACK;
      loop # Suche im Stack ab FRAME nach einem CATCH-Frame mit demselben Tag:
        { if (eq(FRAME_(0),nullobj)) # Stackende?
            { return; } # ja -> kein passendes Catch vorhanden -> Rücksprung
          if (framecode(FRAME_(0)) & bit(frame_bit_t))
            # Frame gefunden
            { if ((framecode(FRAME_(0)) == CATCH_frame_info) # Catch-Frame?
                  && eq(FRAME_(frame_tag),tag) # mit demselben Tag?
                 )
                break; # ja -> Suchschleife fertig
              # Frame übergehen:
              FRAME = topofframe(FRAME_(0));
            }
            else
            { FRAME skipSTACKop 1; }
        }
      # FRAME zeigt auf den untersten CATCH-Frame mit demselben Tag
      unwind_upto(FRAME); # bis dorthin auflösen, dann anspringen
    }

# UP: Ruft alle Handler zur Condition cond auf. Kommt nur zurück, wenn keiner
# dieser Handler sich zuständig fühlt (d.h. wenn jeder Handler zurückkehrt).
# invoke_handlers(cond);
# can trigger GC
  global void invoke_handlers (object cond);
# Dies deaktiviert den Handler, der gerade aufgerufen wird,
# und alle neueren Handler.
  global void invoke_handlers(cond)
    var object cond;
    { # Die Handler-Bereiche, die ausgeblendet werden:
      var stack_range* other_ranges = inactive_handlers;
      var stack_range new_range;
      # Suche nach Handler-Frame, der einen Typ behandelt mit (TYPEP cond type):
     {var object* FRAME = STACK;
      loop # Suche im Stack ab FRAME nach einem passenden HANDLER-Frame:
        { if (!(other_ranges == NULL) && (FRAME == other_ranges->low_limit))
            { FRAME = other_ranges->high_limit;
              other_ranges = other_ranges->next;
            }
          elif (eq(FRAME_(0),nullobj)) # Stackende?
            { break; } # ja -> fertig, Rücksprung
          elif (framecode(FRAME_(0)) & bit(frame_bit_t))
            # Frame gefunden
            { if (framecode(FRAME_(0)) == HANDLER_frame_info) # Handler-Frame?
                # Typen des Vektors #(type1 label1 ... typem labelm) durchlaufen:
                { var uintL m2 = Svector_length(Car(FRAME_(frame_handlers))); # 2*m
                  var uintL i = 0;
                  do { pushSTACK(cond); # cond retten
                       pushSTACK(cond);
                       pushSTACK(TheSvector(Car(FRAME_(frame_handlers)))->data[i]); # typei
                       funcall(S(safe_typep),2); # (SYS::SAFE-TYPEP cond typei) ausführen
                       if (!nullp(value1)) # passender Handler gefunden?
                         goto found_handler;
                       cond = popSTACK(); # cond zurück
                       i += 2;
                     }
                     while (i < m2);
                  if (FALSE)
                    found_handler:
                    { # CLtL2 S. 873, 884: "A handler is executed in the dynamic context
                      # of the signaler, except that the set of available condition
                      # handlers will have been rebound to the value that was active
                      # at the time the condition handler was made active."
                      # Das Ganze sichern wir durch einen Unwind-Protect-Frame ab:
                      var stack_range* saved_inactive_handlers = inactive_handlers;
                      new_range.low_limit = STACK;
                      new_range.high_limit = topofframe(FRAME_(0));
                      new_range.next = other_ranges;
                      {var object* top_of_frame = STACK;
                       var sp_jmp_buf returner; # Rücksprungpunkt
                       finish_entry_frame(UNWIND_PROTECT,&!returner,,
                         { var restart fun = unwind_protect_to_save.fun;
                           var object* arg = unwind_protect_to_save.upto_frame;
                           skipSTACK(2); # Unwind-Protect-Frame auflösen
                           # Cleanup: Handler reaktivieren:
                           inactive_handlers = saved_inactive_handlers;
                           # und weiterspringen:
                           fun(arg);
                           NOTREACHED
                         });
                       # Handler deaktivieren:
                       inactive_handlers = &new_range;
                       if (!nullp(Cdr(FRAME_(frame_handlers))))
                         { # Information für den Handler bereitlegen:
                           handler_args.condition = STACK_(0+2);
                           handler_args.stack = FRAME STACKop 4;
                           handler_args.sp = (SPint*)(aint)as_oint(FRAME_(frame_SP));
                           handler_args.spdepth = Cdr(FRAME_(frame_handlers));
                           # Handler aufrufen:
                          {var object closure = FRAME_(frame_closure);
                           var object codevec = TheCclosure(closure)->clos_codevec;
                           var uintL index = (TheCodevec(codevec)->ccv_flags & bit(7) ? CCV_START_KEY : CCV_START_NONKEY)
                                             + posfixnum_to_L(TheSvector(Car(FRAME_(frame_handlers)))->data[i+1]);
                           interpret_bytecode(closure,codevec,index);
                         }}
                         else
                         { # C-Handler aufrufen:
                           void* handler_fn = TheMachineCode(FRAME_(frame_closure));
                           ((void (*) (void*, object*, object, object)) handler_fn)
                             ((void*)(aint)as_oint(FRAME_(frame_SP)),
                              FRAME,
                              TheSvector(Car(FRAME_(frame_handlers)))->data[i+1],
                              STACK_(0+2)
                             );
                         }
                       skipSTACK(2); # Unwind-Protect-Frame auflösen
                       # Handler reaktivieren:
                       inactive_handlers = saved_inactive_handlers;
                      }
                      cond = popSTACK(); # cond zurück
                }   }
              # Frame übergehen:
              FRAME = topofframe(FRAME_(0));
            }
            else
            { FRAME skipSTACKop 1; }
    }}  }

# UP: Stellt fest, ob ein Objekt ein Funktionsname, d.h. ein Symbol oder
# eine Liste der Form (SETF symbol), ist.
# funnamep(obj)
# > obj: Objekt
# < ergebnis: TRUE falls Funktionsname
  global boolean funnamep (object obj);
  global boolean funnamep(obj)
    var object obj;
    { if (symbolp(obj)) return TRUE;
      if (consp(obj) && eq(Car(obj),S(setf)))
          { obj = Cdr(obj);
            if (consp(obj) && nullp(Cdr(obj)) && symbolp(Car(obj)))
              return TRUE;
          }
      return FALSE;
    }

# UP: Liefert den Wert eines Symbols in einem Environment.
# sym_value(symbol,venv)
# > symbol: Symbol
# > venv: ein Variablen- und Symbolmacro-Environment
# < ergebnis: Wert des Symbols in diesem Environment
  local object sym_value (object sym, object venv);
  local object sym_value(sym,env)
    var object sym;
    var object env;
    { if (constantp(TheSymbol(sym))) goto global_value; # Konstanten haben nur globale Werte
      if (special_var_p(TheSymbol(sym))) goto global_value; # special deklarierte ebenso
     {
      #ifdef NO_symbolflags
        #define binds_sym_p(bindptr) # Bindet die Bindung bei bindptr das Symbol sym? \
          (eq(*(bindptr STACKop 1),sym) # richtiges Symbol?                                  \
           && eq(*(bindingsptr STACKop 0),fixnum(bit(active_bit))) # und aktiv und statisch? \
          )
      #else
      var object cmp = as_object(as_oint(sym) | wbit(active_bit_o)); # zum Vergleich: Bindung muss aktiv sein
        #define binds_sym_p(bindptr) # Bindet die Bindung bei bindptr das Symbol sym? \
          (eq(*(bindingsptr STACKop 0),cmp)) # richtiges Symbol und aktiv und statisch?
      #endif
      next_env:
        if (framepointerp(env))
          # Environment ist ein Pointer auf einen Variablenbindungs-Frame
          { var object* FRAME = TheFramepointer(env);
           {var uintL count = as_oint(FRAME_(frame_anz)); # Anzahl der Bindungen
            if (count > 0)
              { var object* bindingsptr = &FRAME_(frame_bindings); # Pointer auf die erste Bindung
                dotimespL(count,count,
                  { if (binds_sym_p(bindingsptr)) # richtiges Symbol und aktiv und statisch?
                      { var object value = *(bindingsptr STACKop varframe_binding_value);
                        if (eq(value,specdecl))
                          { goto global_value; }
                          else
                          { return value; }
                      }
                    bindingsptr skipSTACKop varframe_binding_size; # nein: nächste Bindung
                  });
              }
            env = FRAME_(frame_next_env);
            goto next_env;
          }}
        elif (simple_vector_p(env))
          # Environment ist ein Simple-Vector
          goto next_vector;
        else
          # Environment ist NIL
          goto global_value;
      next_vector:
        # Environment ist ein Simple-Vector
        { var uintL count = floor(Svector_length(env),2); # Anzahl der Bindungen
          var object* ptr = &TheSvector(env)->data[0];
          dotimesL(count,count,
            { if (eq(*ptr,sym)) # richtiges Symbol?
                { var object value = *(ptr+1);
                  if (eq(value,specdecl))
                    { goto global_value; }
                    else
                    { return value; }
                }
              ptr += 2; # nächste Bindung
            });
          env = *ptr; # nächstes Environment
          if (simple_vector_p(env)) goto next_vector; # ein Simple-Vector?
          # sonst: Environment ist NIL
        }
      #undef binds_sym_p
     }
      global_value: # Es gilt der globale (dynamische) Wert des Symbols
        return Symbol_value(sym);
    }

# UP: Stellt fest, ob ein Symbol im aktuellen Environment einen Macro darstellt.
# sym_macrop(symbol)
# > symbol: Symbol
# < ergebnis: TRUE falls sym einen Symbol-Macro darstellt
  global boolean sym_macrop (object sym);
  global boolean sym_macrop(sym)
    var object sym;
    { var object val = sym_value(sym,aktenv.var_env);
      return (symbolmacrop(val) ? TRUE : FALSE);
    }

# UP: Setzt den Wert eines Symbols im aktuellen Environment.
# setq(symbol,value);
# > symbol: Symbol, keine Konstante
# > value: gewünschter Wert des Symbols im aktuellen Environment
  global void setq (object sym, object value);
  global void setq(sym,value)
    var object sym;
    var object value;
    { if (special_var_p(TheSymbol(sym))) goto global_value; # special deklarierte ebenso
     {var object env = aktenv.var_env; # aktuelles VAR_ENV
      #ifdef NO_symbolflags
        #define binds_sym_p(bindptr) # Bindet die Bindung bei bindptr das Symbol sym? \
          (eq(*(bindptr STACKop 1),sym) # richtiges Symbol?                                  \
           && eq(*(bindingsptr STACKop 0),fixnum(bit(active_bit))) # und aktiv und statisch? \
          )
      #else
      var object cmp = as_object(as_oint(sym) | wbit(active_bit_o)); # zum Vergleich: Bindung muss aktiv sein
        #define binds_sym_p(bindptr) # Bindet die Bindung bei bindptr das Symbol sym? \
          (eq(*(bindingsptr STACKop 0),cmp)) # richtiges Symbol und aktiv und statisch?
      #endif
      next_env:
        if (framepointerp(env))
          # Environment ist ein Pointer auf einen Variablenbindungs-Frame
          { var object* FRAME = TheFramepointer(env);
           {var uintL count = as_oint(FRAME_(frame_anz)); # Anzahl der Bindungen
            if (count > 0)
              { var object* bindingsptr = &FRAME_(frame_bindings); # Pointer auf die erste Bindung
                dotimespL(count,count,
                  { if (binds_sym_p(bindingsptr)) # richtiges Symbol und aktiv und statisch?
                      { if (eq(*(bindingsptr STACKop varframe_binding_value),specdecl))
                          { goto global_value; }
                          else
                          { *(bindingsptr STACKop varframe_binding_value) = value; return; }
                      }
                    bindingsptr skipSTACKop varframe_binding_size; # nein: nächste Bindung
                  });
              }
            env = FRAME_(frame_next_env);
            goto next_env;
          }}
        elif (simple_vector_p(env))
          # Environment ist ein Simple-Vector
          goto next_vector;
        else
          # Environment ist NIL
          goto global_value;
      next_vector:
        # Environment ist ein Simple-Vector
        { var uintL count = floor(Svector_length(env),2); # Anzahl der Bindungen
          var object* ptr = &TheSvector(env)->data[0];
          dotimesL(count,count,
            { if (eq(*ptr,sym)) # richtiges Symbol?
                { if (eq(*(ptr+1),specdecl))
                    { goto global_value; }
                    else
                    { *(ptr+1) = value; return; }
                }
              ptr += 2; # nächste Bindung
            });
          env = *ptr; # nächstes Environment
          if (simple_vector_p(env)) goto next_vector; # ein Simple-Vector?
          # sonst: Environment ist NIL
        }
      #undef binds_sym_p
     }
      global_value: # Es gilt der globale (dynamische) Wert des Symbols
        Symbol_value(sym) = value; return;
    }

# UP: Liefert zu einem Symbol seine Funktionsdefinition in einem Environment
# sym_function(sym,fenv)
# > sym: Funktionsname (z.B. Symbol)
# > fenv: ein Funktions- und Macrobindungs-Environment
# < ergebnis: Funktionsdefinition, entweder unbound (falls undefinierte Funktion)
#             oder Closure/SUBR/FSUBR oder ein Cons (SYS::MACRO . expander).
  global object sym_function (object sym, object fenv);
  global object sym_function(sym,env)
    var object sym;
    var object env;
    { var object value;
     {next_env:
        if (framepointerp(env))
          # Environment ist ein Pointer auf einen Funktionsbindungs-Frame
          { var object* FRAME = TheFramepointer(env);
           {var uintL count = as_oint(FRAME_(frame_anz)); # Anzahl der Bindungen
            if (count > 0)
              { var object* bindingsptr = &FRAME_(frame_bindings); # Pointer auf die erste Bindung
                dotimespL(count,count,
                  { if (equal(*(bindingsptr STACKop 0),sym)) # richtiges Symbol?
                      { value = *(bindingsptr STACKop 1); goto fertig; }
                    bindingsptr skipSTACKop 2; # nein: nächste Bindung
                  });
              }
            env = FRAME_(frame_next_env);
            goto next_env;
          }}
        elif (simple_vector_p(env))
          # Environment ist ein Simple-Vector
          goto next_vector;
        else
          # Environment ist NIL
          goto global_value;
      next_vector:
        # Environment ist ein Simple-Vector
        { var uintL count = floor(Svector_length(env),2); # Anzahl der Bindungen
          var object* ptr = &TheSvector(env)->data[0];
          dotimesL(count,count,
            { if (equal(*ptr,sym)) # richtiges Symbol?
                { value = *(ptr+1); goto fertig; }
              ptr += 2; # nächste Bindung
            });
          env = *ptr; # nächstes Environment
          if (simple_vector_p(env)) goto next_vector; # ein Simple-Vector?
          # sonst: Environment ist NIL
        }
     }
      global_value: # Es gilt die globale Funktionsdefinition
        if (!symbolp(sym))
          { sym = get(Car(Cdr(sym)),S(setf_function)); # (get ... 'SYS::SETF-FUNCTION)
            if (!symbolp(sym)) # sollte (uninterniertes) Symbol sein
              { return unbound; } # sonst undefiniert
          }
        return Symbol_function(sym);
      fertig: # Symbol aktiv im Environment gefunden, "Wert" value
        # (eine Closure oder NIL oder ein Cons (SYS::MACRO . expander) )
        # Falls Definition = NIL (während LABELS), gilt die Funktion als
        # undefiniert:
        if (nullp(value)) { value = unbound; }
        return value;
    }

# UP: Wertet eine Form in einem gegebenen Environment aus.
# eval_5env(form,var,fun,block,go,decl);
# > var_env: Wert für VAR_ENV
# > fun_env: Wert für FUN_ENV
# > block_env: Wert für BLOCK_ENV
# > go_env: Wert für GO_ENV
# > decl_env: Wert für DECL_ENV
# > form: Form
# < mv_count/mv_space: Werte
# can trigger GC
  global Values eval_5env (object form, object var_env, object fun_env, object block_env, object go_env, object decl_env);
  global Values eval_5env(form,var_env,fun_env,block_env,go_env,decl_env)
    var object form;
    var object var_env;
    var object fun_env;
    var object block_env;
    var object go_env;
    var object decl_env;
    { # Environments binden:
      make_ENV5_frame();
      # aktuelle Environments setzen:
      aktenv.var_env = var_env;
      aktenv.fun_env = fun_env;
      aktenv.block_env = block_env;
      aktenv.go_env = go_env;
      aktenv.decl_env = decl_env;
      # Form auswerten:
      eval(form);
      # Environment-Frame auflösen:
      unwind();
      return; # fertig
    }

# UP: Wertet eine Form in einem leeren Environment aus.
# eval_noenv(form);
# > form: Form
# < mv_count/mv_space: Werte
# can trigger GC
  global Values eval_noenv (object form);
  global Values eval_noenv(form)
    var object form;
    { return_Values eval_5env(form,NIL,NIL,NIL,NIL,O(top_decl_env)); }

# UP: "nestet" ein FUN-Environment, d.h. schreibt alle aktiven Bindungen
# aus dem Stack in neu allozierte Vektoren.
# nest_fun(env)
# > env: FUN-Env
# < ergebnis: selbes Environment, kein Pointer in den Stack
# can trigger GC
  global object nest_fun (object env);
  global object nest_fun(env)
    var object env;
    { var uintL depth = 0; # Rekursionszähler:=0
      # Pseudorekursion mit Input env, Output env.
      nest_start: # Rekursionsbeginn
      if (framepointerp(env))
        # env ist ein Pointer auf einen STACK-Frame.
        { check_STACK();
          pushSTACK(env); # env retten
          # entrekursiviert nest_fun(NEXT_ENV(env)) durchführen:
          {var object* FRAME = TheFramepointer(env);
           env = FRAME_(frame_next_env); depth++; goto nest_start;
          }
          nest_reentry: depth--;
          # NEXT_ENV ist nun genestet.
          {var object* FRAME = TheFramepointer(STACK_0); # nächster zu nestender STACK-Frame
           STACK_0 = env; # bisher genestetes Environment
           {var uintL anzahl = as_oint(FRAME_(frame_anz)); # Anzahl der noch nicht genesteten Bindungen
            if (anzahl == 0)
              # keine Bindungen -> unnötig, einen Vektor zu erzeugen.
              { env = popSTACK(); }
              else
              # Vektor für anzahl Bindungen erzeugen:
              { env = allocate_vector(2*anzahl+1);
                # und füllen:
                { var object* ptr = &TheSvector(env)->data[0];
                  var object* bindingsptr = &FRAME_(frame_bindings); # Pointer auf die erste Bindung
                  # anzahl Bindungen ab bindingsptr in den Vektor ab ptr eintragen:
                  dotimespL(anzahl,anzahl,
                    { *ptr++ = *(bindingsptr STACKop 0); # Bindung in den Vektor kopieren
                      *ptr++ = *(bindingsptr STACKop 1);
                      bindingsptr skipSTACKop 2;
                    });
                  *ptr++ = popSTACK(); # genestetes NEXT_ENV in Vektor eintragen
                }
                FRAME_(frame_next_env) = env; # Vektor als NEXT_ENV in den Frame
                FRAME_(frame_anz) = as_object(0); # neue Zahl noch nicht genesteter Bindungen
              }
        } }}
      # mit diesem Nest-Teilschritt fertig.
      if (depth>0) goto nest_reentry; # Ende der Rekursion
      return env;
    }

# UP: "nestet" ein VAR-Environment, d.h. schreibt alle aktiven Bindungen
# aus dem Stack in neu allozierte Vektoren.
# nest_var(env)
# > env: VAR-Env
# < ergebnis: selbes Environment, kein Pointer in den Stack
# can trigger GC
  local object nest_var (object env);
  local object nest_var(env)
    var object env;
    { var uintL depth = 0; # Rekursionszähler:=0
      # Pseudorekursion mit Input env, Output env.
      nest_start: # Rekursionsbeginn
      if (framepointerp(env))
        # env ist ein Pointer auf einen STACK-Frame.
        { check_STACK();
          pushSTACK(env); # env retten
          # entrekursiviert nest_var(NEXT_ENV(env)) durchführen:
          {var object* FRAME = TheFramepointer(env);
           env = FRAME_(frame_next_env); depth++; goto nest_start;
          }
          nest_reentry: depth--;
          # NEXT_ENV ist nun genestet.
          {var object* FRAME = TheFramepointer(STACK_0); # nächster zu nestender STACK-Frame
           STACK_0 = env; # bisher genestetes Environment
           # Suche (von unten) die erste aktive unter den noch nicht
           # genesteten Bindungen:
           {var uintL anzahl = as_oint(FRAME_(frame_anz)); # Anzahl der noch nicht genesteten Bindungen
            var uintL count = 0;
            var object* bindingsptr = &FRAME_(frame_bindings); # Pointer auf die erste Bindung
            until ((count>=anzahl) # alle ungenesteten Bindungen durch?
                   || (as_oint(*(bindingsptr STACKop 0)) & wbit(active_bit_o)) # aktive Bindung entdeckt?
                  )
              { # nein -> weitersuchen:
                bindingsptr skipSTACKop varframe_binding_size;
                count++;
              }
            # Unterhalb von bindingsptr sind count inaktive Bindungen.
            # Ab bindingsptr kommen anzahl-count aktive, zu nestende Bindungen.
            anzahl = anzahl-count; # Anzahl zu nestender Bindungen
            if (anzahl == 0)
              # keine Bindungen -> unnötig, einen Vektor zu erzeugen.
              { env = popSTACK(); }
              else
              # Vektor für anzahl Bindungen erzeugen:
              { env = allocate_vector(2*anzahl+1);
                # und füllen:
                { var object* ptr = &TheSvector(env)->data[0];
                  # Bindungen ab bindingsptr in den Vektor ab ptr eintragen:
                  dotimespL(anzahl,anzahl,
                    { if (as_oint(*(bindingsptr STACKop varframe_binding_mark)) & wbit(dynam_bit_o)) # Bindung dynamisch?
                        # dynamische Bindung, lexikalische Sichtbarkeit
                        { *ptr++ = symbol_without_flags(*(bindingsptr STACKop varframe_binding_sym)); # Symbol ohne Flag-Bits in den Vektor
                          *ptr++ = specdecl; # als special reference kennzeichnen
                          # Bindung bleibt im Frame aktiv
                        }
                        else
                        # statische Bindung, lexikalische Sichtbarkeit
                        { *(bindingsptr STACKop varframe_binding_mark) =
                            as_object(as_oint(*(bindingsptr STACKop varframe_binding_mark)) & ~wbit(active_bit_o)); # Bindung inaktivieren
                          *ptr++ = *(bindingsptr STACKop varframe_binding_sym); # Bindung in den Vektor kopieren
                          *ptr++ = *(bindingsptr STACKop varframe_binding_value);
                        }
                      bindingsptr skipSTACKop varframe_binding_size;
                    });
                  *ptr++ = popSTACK(); # genestetes NEXT_ENV in Vektor eintragen
                }
                FRAME_(frame_next_env) = env; # Vektor als NEXT_ENV in den Frame
                FRAME_(frame_anz) = as_object(count); # neue Zahl noch nicht genesteter Bindungen
              }
        } }}
      # mit diesem Nest-Teilschritt fertig.
      if (depth>0) goto nest_reentry; # Ende der Rekursion
      return env;
    }

# UP: Nestet die Environments in *env (d.h. schreibt alle Informationen in
# Stack-unabhängige Strukturen) und schiebt sie auf den STACK.
# (Die Werte VAR_ENV, FUN_ENV, BLOCK_ENV, GO_ENV, DECL_ENV werden nicht
# verändert, da evtl. noch inaktive Bindungen in Frames sitzen, die ohne
# Veränderung von VAR_ENV aktiviert werden können müssen.)
# nest_env(env)
# > environment* env: Pointer auf fünf einzelne Environments
# < environment* ergebnis: Pointer auf die Environments im STACK
# verändert STACK, kann GC auslösen
  global environment* nest_env (environment* env);
  global environment* nest_env(env5)
    var environment* env5;
    { # Erst alle Environments in den STACK kopieren:
      make_STACK_env(env5->var_env,env5->fun_env,env5->block_env,env5->go_env,env5->decl_env,
                     env5 = );
      # DECL_ENV: Nicht zu verändern.
      # GO_ENV:
      { var object env = env5->go_env;
        var uintL depth = 0; # Rekursionstiefe := 0
        # Pseudo-Rekursion: nestet ein GO_ENV.
        # Input: env, ein GO_ENV. Output: env, die Aliste dazu.
        nest_go_start: # Rekursionsbeginn
        if (framepointerp(env))
          # env ist ein Pointer in den STACK auf einen ITAGBODY-Frame.
          { check_STACK();
           {var object* FRAME = TheFramepointer(env);
            if (framecode(FRAME_(0)) & bit(nested_bit_t)) # Frame schon genestet?
              { env = FRAME_(frame_next_env); } # ja -> bisherige Aliste holen
              else
              {  pushSTACK(env); # env retten
                 # entrekursiviert nest_go(NEXT_ENV(env)) durchführen:
                 env = FRAME_(frame_next_env); depth++; goto nest_go_start;
                 nest_go_reentry: depth--;
                 # NEXT_ENV ist nun genestet.
               { var object frame = STACK_0; # nächster zu nestender STACK-Frame
                 FRAME = uTheFramepointer(frame);
                 STACK_0 = env; # bisher genestetes Environment
                {var object* tagsptr = &FRAME_(frame_bindings); # Pointer aufs unterste Tag
                 var object* frame_end = STACKpointable(topofframe(FRAME_(0))); # Pointer übern Frame
                 var uintL count = # Anzahl der Tags
                   # Dazu die Pointer tagsptr und frame_end (beide ohne Typinfo!) abziehen:
                   STACK_item_count(tagsptr,frame_end) / 2;
                 # Vektor für count Tags erzeugen:
                 { var object tagvec = allocate_vector(count);
                   # und füllen:
                   if (count > 0)
                     { var object* ptr = &TheSvector(tagvec)->data[0];
                       # Tags ab tagsptr in den Vektor ab ptr eintragen:
                       dotimespL(count,count,
                         { *ptr++ = *(tagsptr STACKop 0);
                           tagsptr skipSTACKop 2;
                         });
                     }
                   pushSTACK(tagvec); # und retten
                 }
                 # Nächstes Alistencons (cons Tag-Vektor Frame-Pointer) erzeugen:
                 { var object new_cons = allocate_cons();
                   Car(new_cons) = STACK_0; # tagvec
                   Cdr(new_cons) = frame;
                   STACK_0 = new_cons;
                 }
                 # und vor die Aliste hängen:
                 env = allocate_cons();
                 Car(env) = popSTACK(); # new_cons
                 Cdr(env) = popSTACK(); # bisherige Aliste
                 FRAME_(frame_next_env) = env; # neues NEXT_ENV eintragen
                 *(oint*)(&FRAME_(0)) |= wbit(nested_bit_o); # Dieser Frame ist nun genestet.
              }}}
          }}
        # mit diesem Nest-Teilschritt fertig.
        if (depth>0) goto nest_go_reentry; # Ende der Rekursion
        env5->go_env = env; # genestetes GO_ENV ablegen
      }
      # BLOCK_ENV:
      { var object env = env5->block_env;
        var uintL depth = 0; # Rekursionstiefe := 0
        # Pseudo-Rekursion: nestet ein BLOCK_ENV.
        # Input: env, ein BLOCK_ENV. Output: env, die Aliste dazu.
        nest_block_start: # Rekursionsbeginn
        if (framepointerp(env))
          # env ist ein Pointer in den STACK auf einen IBLOCK-Frame.
          { check_STACK();
           {var object* FRAME = TheFramepointer(env);
            if (framecode(FRAME_(0)) & bit(nested_bit_t)) # Frame schon genestet?
              { env = FRAME_(frame_next_env); } # ja -> bisherige Aliste holen
              else
              { pushSTACK(env); # env retten
                # entrekursiviert nest_block(NEXT_ENV(env)) durchführen:
                env = FRAME_(frame_next_env); depth++; goto nest_block_start;
                nest_block_reentry: depth--;
                # NEXT_ENV ist nun genestet.
               {var object frame = STACK_0; # nächster zu nestender STACK-Frame
                FRAME = TheFramepointer(frame);
                STACK_0 = env; # bisher genestetes Environment
                # Nächstes Alistencons (cons Block-Name Frame-Pointer) erzeugen:
                { var object new_cons = allocate_cons();
                  Car(new_cons) = FRAME_(frame_name);
                  Cdr(new_cons) = frame;
                  pushSTACK(new_cons);
                }
                # und vor die Aliste hängen:
                env = allocate_cons();
                Car(env) = popSTACK(); # new_cons
                Cdr(env) = popSTACK(); # bisherige Aliste
                FRAME_(frame_next_env) = env; # neues NEXT_ENV eintragen
                *(oint*)(&FRAME_(0)) |= wbit(nested_bit_o); # Dieser Frame ist nun genestet.
              }}
          }}
        # mit diesem Nest-Teilschritt fertig.
        if (depth>0) goto nest_block_reentry; # Ende der Rekursion
        env5->block_env = env; # genestetes BLOCK_ENV ablegen
      }
      # FUN_ENV:
      env5->fun_env = nest_fun(env5->fun_env);
      # VAR_ENV:
      env5->var_env = nest_var(env5->var_env);
      # fertig.
      return env5;
    }

# UP: Nestet die aktuellen Environments (d.h. schreibt alle Informationen in
# Stack-unabhängige Strukturen) und schiebt sie auf den STACK.
# (Die Werte VAR_ENV, FUN_ENV, BLOCK_ENV, GO_ENV, DECL_ENV werden nicht
# verändert, da evtl. noch inaktive Bindungen in Frames sitzen, die ohne
# Veränderung von VAR_ENV aktiviert werden können müssen.)
# nest_aktenv()
# < environment* ergebnis: Pointer auf die Environments im STACK
# verändert STACK, kann GC auslösen
  #define nest_aktenv()  nest_env(&aktenv)

# UP: Ergänzt ein Deklarations-Environment um ein decl-spec.
# augment_decl_env(declspec,env)
# > declspec: Deklarations-Specifier, ein Cons
# > env: Deklarations-Environment
# < ergebnis: neues (evtl. augmentiertes) Deklarations-Environment
# can trigger GC
  global object augment_decl_env (object new_declspec, object env);
  global object augment_decl_env(new_declspec,env)
    var object new_declspec;
    var object env;
    { var object decltype = Car(new_declspec); # Deklarations-Typ
      # Ist dies ein zu beachtender Deklarationstyp?
      # Gibt es in env ein Decl-Spec der Form (DECLARATION ... decltype ...) ?
      # NB: Die Liste O(declaration_types) ist das letzte Decl-Spec in env.
      if (symbolp(decltype))
        { # Alle lokal zu beachtenden Deklarations-Typen durchgehen:
          { var object declspecs = env;
            while (consp(declspecs)) # Alle declspecs aus env durchgehen
              { var object declspec = Car(declspecs);
                if (eq(Car(declspec),S(declaration))) # Deklaration (DECLARATION ...) ?
                  { var object list = Cdr(declspec); # ja -> restliche Liste durchgehen
                    while (consp(list))
                      { if (eq(Car(list),decltype)) # Listenelement = decltype ?
                          goto beachten;
                        list = Cdr(list);
                  }   }
                declspecs = Cdr(declspecs);
          }   }
        }
      # nicht zu beachtende Deklaration.
      return env; # env unverändert lassen
      beachten:
      # eine zu beachtende Deklaration -> env := (cons new_declspec env)
      pushSTACK(env); pushSTACK(new_declspec);
      env = allocate_cons();
      Car(env) = popSTACK(); Cdr(env) = popSTACK();
      return env;
    }

# UP: expandiert eine Form, falls möglich, (nicht jedoch, wenn FSUBR-Aufruf
# oder Symbol) in einem Environment
# macroexp(form,venv,fenv);
# > form: Form
# > venv: ein Variablen- und Symbolmacro-Environment
# > fenv: ein Funktions- und Macrobindungs-Environment
# < value1: die Expansion
# < value2: NIL, wenn nicht expandiert,
#           T, wenn expandiert wurde
# can trigger GC
  global void macroexp (object form, object venv, object fenv);
  global void macroexp(form,venv,fenv)
    var object form;
    var object venv;
    var object fenv;
    { if (consp(form)) # nur Listen können Macro-call sein
        { var object funname = Car(form); # Funktionsname
          if (symbolp(funname))
            { var object fdef = sym_function(funname,fenv); # Funktionsdefinition holen
              # Ist sie (SYS::MACRO . Expander) ?
              if (consp(fdef) && eq(Car(fdef),S(macro)))
                # ja -> expandieren:
                { # (FUNCALL *MACROEXPAND-HOOK* expander form env) ausführen:
                  pushSTACK(Cdr(fdef)); # Expander als erstes Argument
                  pushSTACK(form); # Form als zweites Argument
                  pushSTACK(fenv);
                  pushSTACK(nest_var(venv)); # genestetes Variablen- und Symbolmacro-Environment
                  STACK_1 = nest_fun(STACK_1); # genestetes Funktions- und Macrobindungs-Environment
                 {var object env = allocate_vector(2); # Environment für beide
                  TheSvector(env)->data[0] = popSTACK(); # venv als 1. Komponente
                  TheSvector(env)->data[1] = STACK_0;    # fenv als 2. Komponente
                  STACK_0 = env; # Environment als drittes Argument
                  funcall(Symbol_value(S(macroexpand_hook)),3);
                  value2 = T; # expandierte Form als 1. Wert, T als 2. Wert
                  return;
                }}
        }   }
      # sonst nicht expandieren:
      value1 = form; value2 = NIL;
    }

# UP: expandiert eine Form, falls möglich, (auch, wenn FSUBR-Aufruf)
# in einem Environment
# macroexp0(form,env);
# > form: Form
# > env: ein Macroexpansions-Environment
# < value1: die Expansion
# < value2: NIL, wenn nicht expandiert,
#           T, wenn expandiert wurde
# can trigger GC
  global void macroexp0 (object form, object env);
  global void macroexp0(form,env)
    var object form;
    var object env;
    { if (consp(form)) # nur Listen können Macro-call sein
        { var object funname = Car(form); # Funktionsname
          if (symbolp(funname))
            { var object fdef = sym_function(funname,TheSvector(env)->data[1]); # Funktionsdefinition holen
              if (fsubrp(fdef))
                # fdef ist ein FSUBR, also war die globale Funktionsdefinition gültig.
                # Schaue nach, ob die Propertyliste eine Macrodefinition enthält:
                { var object expander = get(funname,S(macro)); # nach Property SYS::MACRO suchen
                  if (!eq(expander,unbound))
                    # gefunden. Mit dem Expander aus der Propertyliste expandieren:
                    { # (FUNCALL *MACROEXPAND-HOOK* expander form env) ausführen:
                      pushSTACK(expander); # Expander als erstes Argument
                      pushSTACK(form); # Form als zweites Argument
                      pushSTACK(env); # Environment als drittes Argument
                      funcall(Symbol_value(S(macroexpand_hook)),3);
                      value2 = T; # expandierte Form als 1. Wert, T als 2. Wert
                      return;
                }   }
                else
                # 3 Möglichkeiten:
                # #UNBOUND/SUBR/Closure (globale oder lexikalische Funktionsdef.)
                #   -> nicht expandieren
                # (SYS::MACRO . Expander) (lexikalische Macrodefinition)
                #   -> expandieren (Expander aufrufen)
                # Symbol (lexikalische Funktionsdefinition während SYS::%EXPAND)
                #   expandieren: (list* 'SYS::%FUNCALL Symbol (cdr form))
                if (consp(fdef))
                  { # Ist es (SYS::MACRO . Expander) ?
                    if (eq(Car(fdef),S(macro)))
                      # ja -> expandieren:
                      { # (FUNCALL *MACROEXPAND-HOOK* expander form env) ausführen:
                        pushSTACK(Cdr(fdef)); # Expander als erstes Argument
                        pushSTACK(form); # Form als zweites Argument
                        pushSTACK(env); # Environment als drittes Argument
                        funcall(Symbol_value(S(macroexpand_hook)),3);
                        value2 = T; # expandierte Form als 1. Wert, T als 2. Wert
                        return;
                  }   }
                elif (symbolp(fdef))
                  # fdef ein Symbol
                  { # Muss zu (SYS::%FUNCALL fdef ...) expandieren:
                    pushSTACK(Cdr(form)); # (cdr form)
                    pushSTACK(fdef); # Symbol
                   {var object new_cons = allocate_cons();
                    Car(new_cons) = popSTACK(); Cdr(new_cons) = STACK_0;
                    STACK_0 = new_cons; # (cons Symbol (cdr form))
                   }
                   {var object new_cons = allocate_cons();
                    Car(new_cons) = S(pfuncall); Cdr(new_cons) = popSTACK();
                    value1 = new_cons; # (cons 'SYS::%FUNCALL (cons Symbol (cdr form)))
                   }
                    value2 = T; return; # es wurde expandiert.
                  }
        }   }
      elif (symbolp(form))
        { var object val = sym_value(form,TheSvector(env)->data[0]);
          if (symbolmacrop(val)) # Symbol-Macro gefunden?
            # ja -> expandieren
            { value1 = TheSymbolmacro(val)->symbolmacro_expansion; value2 = T; return; }
        }
      # sonst nicht expandieren:
      value1 = form; value2 = NIL;
    }

# UP: Parse-Declarations-Docstring. Trennt von einer Formenliste diejenigen
# ab, die als Deklarationen bzw. Dokumentationsstring angesehen werden
# müssen.
# parse_dd(formlist,venv,fenv)
# > formlist: ( {decl|doc-string} . body )
# > venv: ein Variablen- und Symbolmacro-Environment (für die Macroexpansionen)
# > fenv: Funktions- und Macrobindungs-Environment (für die Macroexpansionen)
# < value1: body
# < value2: Liste der decl-specs
# < value3: Doc-String oder NIL
# < ergebnis: TRUE falls eine (COMPILE)-Deklaration vorkam, FALSE sonst
# can trigger GC
  global boolean parse_dd (object formlist, object venv, object fenv);
  global boolean parse_dd(formlist,venv,fenv)
    var object formlist;
    var object venv;
    var object fenv;
    { pushSTACK(formlist); # formlist aufheben für Fehlermeldung
      pushSTACK(venv); # Variablen-Environment
      pushSTACK(fenv); # Macrobindungs-Environment
      pushSTACK(NIL); # vorläufiger Doc-String
      pushSTACK(NIL); # Anfang decl-spec-Liste
      # Stackaufbau: formlist, venv, fenv, docstring, declspecs.
     {var boolean compile_decl = FALSE; # Flag, ob eine (COMPILE)-Deklaration vorkam
      var object body = formlist; # Rest der Formenliste
      while (consp(body))
        {  pushSTACK(body); # body retten
         { var object form = Car(body); # nächste Form
           # evtl. macroexpandieren (ohne FSUBRs, Symbole zu expandieren):
           do { macroexp(form,STACK_(3+1),STACK_(2+1)); form = value1; }
              until (nullp(value2));
           body = popSTACK();
          {var object body_rest = Cdr(body); # body verkürzen
           if (stringp(form)) # Doc-String gefunden?
             { if (atomp(body_rest)) # an letzter Stelle der Formenliste?
                 goto fertig; # ja -> letzte Form kann kein Doc-String sein!
               if (!nullp(STACK_1)) # schon ein Doc-String dagewesen?
                 # ja -> mehr als ein Doc-String ist zuviel:
                 { pushSTACK(STACK_4); # formlist
                   fehler(source_program_error,
                          GETTEXT("Too many documentation strings in ~")
                         );
                 }
               STACK_1 = form; # neuer Doc-String
               body = body_rest;
             }
           elif (consp(form) && eq(Car(form),S(declare))) # Deklaration (DECLARE ...) ?
             { # neue decl-specs einzeln auf STACK_0 consen:
               pushSTACK(body_rest); # body_rest retten
               pushSTACK(Cdr(form)); # Liste der neuen decl-specs
               while (mconsp(STACK_0))
                 {{var object declspec = Car(STACK_0); # nächstes decl-spec
                   # Teste, ob (EQUAL d '(COMPILE)) =
                   #   (and (consp d) (eq (car d) 'COMPILE) (null (cdr d)))
                   if (consp(declspec)
                       && eq(Car(declspec),S(compile))
                       && nullp(Cdr(declspec))
                      )
                     { compile_decl = TRUE; }
                  }# Diese Deklaration auf STACK_(0+2) consen:
                  {var object new_cons = allocate_cons();
                   Car(new_cons) = Car(STACK_0);
                   Cdr(new_cons) = STACK_(0+2);
                   STACK_(0+2) = new_cons;
                  }# zum nächsten decl-spec:
                   STACK_0 = Cdr(STACK_0);
                 }
               skipSTACK(1);
               body = popSTACK(); # body := alter body_rest
             }
           else
             { fertig: # fertig mit Durchlaufen der Formenliste
               #if 0
               # Das war einmal eine schöne Optimierung, die zweimaliges
               # Macroexpandieren vermied. Leider ist sie nicht mehr sicher,
               # denn bei (FUNCTION (LAMBDA ...)), LET, LET*, MULTIPLE-VALUE-BIND
               # wird das äußere(!) Variablen-Environment übergeben, so dass in
               # (SYMBOL-MACROLET ((X Y)) (LET ((X (FOO))) (SETF X ...) ...))
               # der SETF-Macro ein verkehrtes venv übergeben bekäme und zu
               # (SETQ Y ...) expandieren würde.
               if (!eq(form,Car(body))) # sofern die Form expandiert wurde,
                 # ersetze body durch (cons form (cdr body)) :
                 { pushSTACK(body_rest); pushSTACK(form);
                   body = allocate_cons();
                   Car(body) = popSTACK(); # form
                   Cdr(body) = popSTACK(); # body_rest
                 }
               #endif
               break;
             }
        }}}
      value1 = body;
      value2 = nreverse(popSTACK()); # decl-spec-Liste
      value3 = popSTACK(); # Doc-String
      skipSTACK(3);
      return compile_decl;
    }}

# UP: bindet *EVALHOOK* und *APPLYHOOK* dynamisch an die gegebenen Werte.
# bindhooks(evalhook_value,applyhook_value);
# > evalhook_value: Wert für *EVALHOOK*
# > applyhook_value: Wert für *APPLYHOOK*
# verändert STACK
  global void bindhooks (object evalhook_value, object applyhook_value);
  global void bindhooks(evalhook_value,applyhook_value)
    var object evalhook_value;
    var object applyhook_value;
    { # Frame aufbauen:
      { var object* top_of_frame = STACK; # Pointer übern Frame
        pushSTACK(Symbol_value(S(evalhookstern)));  # alter Wert von *EVALHOOK*
        pushSTACK(S(evalhookstern));                # *EVALHOOK*
        pushSTACK(Symbol_value(S(applyhookstern))); # alter Wert von *APPLYHOOK*
        pushSTACK(S(applyhookstern));               # *APPLYHOOK*
        finish_frame(DYNBIND);
      }
      # Frame fertig aufgebaut, nun die Werte der Variablen verändern:
      Symbol_value(S(evalhookstern)) = evalhook_value; # (SETQ *EVALHOOK* evalhook_value)
      Symbol_value(S(applyhookstern)) = applyhook_value; # (SETQ *APPLYHOOK* applyhook_value)
    }

# UP: bindet *EVALHOOK* und *APPLYHOOK* dynamisch an NIL.
# bindhooks_NIL();
# verändert STACK
  #define bindhooks_NIL()  bindhooks(NIL,NIL)

# UP: Bestimmt den Source-Lambdabody eines Lambdabody.
# lambdabody_source(lambdabody)
# > lambdabody: Lambdabody (ein Cons)
# < ergebnis: Source-Lambdabody (unbound falls keine Source angegeben)
  local object lambdabody_source (object lambdabody);
  local object lambdabody_source(lambdabody)
    var object lambdabody;
    { var object body = Cdr(lambdabody);
      # body = ((DECLARE (SOURCE ...) ...) ...) ?
      if (consp(body))
        { var object form = Car(body); # erste Form
          # form = (DECLARE (SOURCE ...) ...) ?
          if (consp(form) && eq(Car(form),S(declare)))
            { var object declspecs = Cdr(form);
              # declspecs = ((SOURCE ...) ...) ?
              if (consp(declspecs))
                { var object declspec = Car(declspecs);
                  # declspec = (SOURCE ...) ?
                  if (consp(declspec) && eq(Car(declspec),S(source)))
                    { var object declspecr = Cdr(declspec);
                      if (consp(declspecr))
                        # Source gefunden
                        { return Car(declspecr); }
        }   }   }   }
      return unbound;
    }

# UP: Fügt einen impliziten BLOCK in einen Lambdabody ein.
# add_implicit_block();
# > STACK_1: Funktionsname
# > STACK_0: Lambdabody
# > value1: body
# > value2: Liste der decl-specs
# > value3: Doc-String oder NIL
# < STACK_0: neuer Lambdabody
# can trigger GC
  local void add_implicit_block (void);
  local void add_implicit_block()
    { # Ersetze lambdabody durch
      # (cons (car lambdabody) (add-implicit-block name (cdr lambdabody))):
      var object new_body;
      pushSTACK(value2); # declarations
      pushSTACK(value3); # docstring
      pushSTACK(funname_blockname(STACK_(1+2))); # blockname
      pushSTACK(value1); # body-rest
      # Stackaufbau: name, lambdabody, declarations, docstring, blockname, body-rest.
      {var object tmp = allocate_cons();
       Cdr(tmp) = popSTACK(); Car(tmp) = STACK_0;
       STACK_0 = tmp;
      }
      {var object tmp = allocate_cons();
       Car(tmp) = S(block); Cdr(tmp) = STACK_0;
       STACK_0 = tmp;
      }
      # Stackaufbau: name, lambdabody, declarations, docstring, block-form.
      {var object tmp = allocate_cons();
       Car(tmp) = popSTACK();
       new_body = tmp;
      }
      # Stackaufbau: name, lambdabody, declarations, docstring.
      if (nullp(STACK_0))
        { skipSTACK(1); }
        else
        { pushSTACK(new_body);
         {var object tmp = allocate_cons();
          Cdr(tmp) = popSTACK(); Car(tmp) = popSTACK();
          new_body = tmp;
        }}
      # Stackaufbau: name, lambdabody, declarations.
      if (nullp(STACK_0))
        { STACK_0 = new_body; }
        else
        { pushSTACK(new_body);
          {var object tmp = allocate_cons();
           Car(tmp) = S(declare); Cdr(tmp) = STACK_1;
           STACK_1 = tmp;
          }
          {var object tmp = allocate_cons();
           Cdr(tmp) = popSTACK(); Car(tmp) = STACK_0;
           STACK_0 = tmp;
        } }
      # Stackaufbau: name, lambdabody, new-body.
      {var object tmp = allocate_cons();
       Cdr(tmp) = popSTACK(); Car(tmp) = Car(STACK_0);
       STACK_0 = tmp;
    } }

# UP: Erzeugt zu einem Lambdabody die entsprechende Closure durch Zerlegen
# der Lambdaliste und eventuelles Macroexpandieren aller Formen.
# get_closure(lambdabody,name,blockp,env)
# > lambdabody: (lambda-list {decl|doc} {form})
# > name: Name, ein Symbol oder (SETF symbol)
# > blockp: ob ein impliziter BLOCK einzuschieben ist
# > env: Pointer auf die fünf einzelnen Environments:
#        env->var_env = VENV, env->fun_env = FENV,
#        env->block_env = BENV, env->go_env = GENV,
#        end->decl_env = DENV.
# < ergebnis: Closure
# can trigger GC
  global object get_closure (object lambdabody, object name, boolean blockp, environment* env);
  global object get_closure(lambdabody,name,blockp,env)
    var object lambdabody;
    var object name;
    var boolean blockp;
    var environment* env;
    { # Lambdabody muss ein Cons sein:
      if (atomp(lambdabody))
        { pushSTACK(name);
          fehler(source_program_error,
                 GETTEXT("FUNCTION: lambda-list for ~ is missing")
                );
        }
      # und der CAR muss eine Liste sein:
      {var object lambdalist = Car(lambdabody);
       if (!listp(lambdalist))
         { pushSTACK(lambdalist);
           pushSTACK(name);
           fehler(source_program_error,
                  GETTEXT("FUNCTION: lambda-list for ~ should be a list, not ~")
                 );
      }  }
      pushSTACK(name);
      pushSTACK(lambdabody);
      # Stackaufbau: name, lambdabody.
      if (parse_dd(Cdr(lambdabody),env->var_env,env->fun_env)) # ({decl|doc} {form}) zerlegen
        # Es trat eine (COMPILE)-Deklaration auf.
        { # Lambdabody durch seine Source ersetzen (denn manche Macros
          # können effizienter compiliert werden als ihre Macro-Expansion):
          { var object source = lambdabody_source(STACK_0);
            if (eq(source,unbound))
              { if (blockp)
                  { add_implicit_block(); }
              }
              else
              { STACK_0 = source; }
          }
          # Environments nesten:
          { var environment* stack_env = nest_env(env); # nesten, auf den STACK legen
            #if !defined(STACK_UP)
            var environment my_env;
            my_env = *stack_env; # und hierher übertragen
            skipSTACK(5); # und wieder vom STACK nehmen
            pushSTACK(my_env.var_env);
            pushSTACK(my_env.fun_env);
            pushSTACK(my_env.block_env);
            pushSTACK(my_env.go_env);
            pushSTACK(my_env.decl_env);
            #endif
            # Stackaufbau: name, lambdabody, venv, fenv, benv, genv, denv.
          }
          # (SYS::COMPILE-LAMBDA name lambdabody venv fenv benv genv denv) ausführen:
          funcall(S(compile_lambda),7);
          return value1; # compilierte Closure als Wert
        }
      # Interpretierte Closure bauen:
      { var object source = lambdabody_source(STACK_0);
        if (eq(source,unbound))
          # keine Source angegeben -> Lambdabody expandieren:
          { if (blockp)
              { add_implicit_block(); }
            # (SYS::%EXPAND-LAMBDABODY-MAIN lambdabody venv fenv) aufrufen:
            pushSTACK(STACK_0); # Lambdabody als 1. Argument
            pushSTACK(nest_var(env->var_env)); # Variablen-Environment genestet als 2. Argument
            pushSTACK(nest_fun(env->fun_env)); # Funktions-Environment genestet als 3. Argument
            funcall(S(expand_lambdabody_main),3);
            lambdabody = value1; # expandierter Lambdabody
          }
          else
          # Source angegeben -> sie ersetzt den alten Lambdabody:
          { lambdabody = STACK_0; # Lambdabody
            STACK_0 = source; # Source-Lambdabody
          }
      }
      # Nun ist  STACK_0     der Source-Lambdabody,
      #          lambdabody  der zu verwendende Lambdabody.
      pushSTACK(Car(lambdabody)); # Lambdaliste
      parse_dd(Cdr(lambdabody),env->var_env,env->fun_env); # ({decl|doc} {form}) zerlegen
      pushSTACK(value1); # Body
      pushSTACK(value2); # Deklarationen
      pushSTACK(value3); # Doc-String oder NIL
     {var object* closure_; # Pointer auf die Closure im STACK
      # Closure erzeugen (mit NIL gefüllt):
      {  var object closure = allocate_closure(iclos_length);
         # und teilweise füllen:
         TheIclosure(closure)->clos_docstring = popSTACK(); # Doc-String
       { var object declarations              = popSTACK(); # Deklarationen
         TheIclosure(closure)->clos_body      = popSTACK(); # Body
        {var object lambdalist                = popSTACK(); # Lambda-Liste
         TheIclosure(closure)->clos_form      = popSTACK(); # Source-Lambdabody
         TheIclosure(closure)->clos_name      = STACK_0;    # Name
         # und retten:
         STACK_0 = closure;
         # Stackaufbau: closure.
         closure_ = &STACK_0; # Pointer auf die Closure im STACK
         pushSTACK(lambdalist); pushSTACK(declarations);
      }}}
      # Environments nesten und genestet in die Closure stecken:
      {var environment* stack_env = nest_env(env);
       var object closure = *closure_;
       TheIclosure(closure)->clos_var_env   = stack_env->var_env  ;
       TheIclosure(closure)->clos_fun_env   = stack_env->fun_env  ;
       TheIclosure(closure)->clos_block_env = stack_env->block_env;
       TheIclosure(closure)->clos_go_env    = stack_env->go_env   ;
       TheIclosure(closure)->clos_decl_env  = stack_env->decl_env ;
       skipSTACK(5);
       TheIclosure(closure)->clos_keywords = Fixnum_0; # keywords:=0, solange &KEY fehlt
      }
      # Stackaufbau: closure, lambdalist, declarations.
      {var uintL spec_count = 0; # Anzahl der dynamischen Referenzen
       var uintL req_count  = 0; # Anzahl der required-Parameter
       var uintL opt_count  = 0; # Anzahl der optional-Parameter
       var uintL key_count  = 0; # Anzahl der Keyword-Parameter
       var uintL aux_count  = 0; # Anzahl der &AUX-Variablen
       var uintL var_count  = 0; # Gesamtzahl der auf dem STACK liegenden Variablen
       {var object declarations = popSTACK();
        # Deklarationen verarbeiten:
        # Dynamisch referenzierte Variablen aus der decl-spec-Liste declarations
        # herauslesen und auf dem STACK ablegen. Sonstige zu beachtende
        # Deklarationen verändern das Deklarations-Environment der Closure.
        while (consp(declarations)) # alle decl-specs abgearbeitet?
          { var object declspec = Car(declarations);
            # declspec muss Liste sein:
            if (atomp(declspec))
              { pushSTACK(declspec);
                fehler(source_program_error,
                       GETTEXT("FUNCTION: illegal declaration ~")
                      );
              }
            # SPECIAL-Deklaration verarbeiten:
            if (eq(Car(declspec),S(special))) # SPECIAL-Deklaration ?
              { var object declspecrest = Cdr(declspec);
                while (consp(declspecrest))
                  { var object sym = Car(declspecrest);
                    if (!symbolp(sym))
                      { pushSTACK(sym);
                        fehler(source_program_error,
                               GETTEXT("FUNCTION: ~ is not a symbol, cannot be declared SPECIAL")
                              );
                      }
                    # Symbol im STACK ablegen:
                    check_STACK(); pushSTACK(sym); spec_count++; var_count++;
                    declspecrest = Cdr(declspecrest);
              }   }
            # sonstige Deklaration verarbeiten:
            pushSTACK(Cdr(declarations)); # declarations verkürzen und retten
            {var object denv = TheIclosure(*closure_)->clos_decl_env;
             denv = augment_decl_env(declspec,denv);
             TheIclosure(*closure_)->clos_decl_env = denv;
            }
            declarations = popSTACK();
       }  }
       {var object lambdalist = *(closure_ STACKop -1); # restliche Lambdaliste
        var object item; # Element der Lambdaliste
        # Macro:
        # NEXT_ITEM(&OPTIONAL_label,&REST_label,&KEY_label,
        #           &ALLOW-OTHER-KEYS_label,&AUX_label,Ende_label)
        # verkürzt den Lambdalistenrest, bringt das nächste Element nach item
        # und springt im Falle eines der 6 angegebenen Lambdalistenmarker an
        # die entsprechenden Stellen.
          #define NEXT_ITEM(opt_label,rest_label,key_label,allow_label,aux_label,end_label)  \
            { if (atomp(lambdalist)) goto end_label; # Lambda-Liste zu Ende?              \
              item = Car(lambdalist); # nächstes Element                                  \
              lambdalist = Cdr(lambdalist); # Liste verkürzen                             \
              if (eq(item,S(LLoptional)))         goto opt_label;   # &OPTIONAL ?         \
              if (eq(item,S(LLrest)))             goto rest_label;  # &REST ?             \
              if (eq(item,S(LLkey)))              goto key_label;   # &KEY ?              \
              if (eq(item,S(LLallow_other_keys))) goto allow_label; # &ALLOW-OTHER-KEYS ? \
              if (eq(item,S(LLaux)))              goto aux_label;   # &AUX ?              \
              if (eq(item,S(LLbody)))             goto badLLkey;    # &BODY ?             \
            }
        req: # required-Parameter abarbeiten und auf dem STACK ablegen:
        loop
          { NEXT_ITEM(opt,rest,key,badLLkey,aux,ende);
            if (!symbolp(item)) goto fehler_symbol;
            if (constantp(TheSymbol(item))) goto fehler_constant;
            # Variable im STACK ablegen:
            check_STACK();
            pushSTACK(item); pushSTACK(Fixnum_0); req_count++; var_count++;
          }
        opt: # &OPTIONAL-Parameter abarbeiten, auf dem STACK ablegen und
             # Init-Formen in die Closure stecken:
        loop
          { NEXT_ITEM(badLLkey,rest,key,badLLkey,aux,ende);
           {var object init_form;
            # Parse Variablenspezifikation in item:
            #   var  oder  (var [init [svar]])
            # Lege var und evtl. svar auf den STACK, setze in var evtl.
            # das svar_bit. Liefert auch init (oder NIL) in init_form.
            check_STACK();
            if (atomp(item))
              { if (!symbolp(item)) goto fehler_symbol;
                if (constantp(TheSymbol(item))) goto fehler_constant;
                # Variable im STACK ablegen:
                pushSTACK(item); pushSTACK(Fixnum_0); opt_count++; var_count++;
                init_form = NIL; # Default-Init
              }
              else
              { var object item_rest = Cdr(item);
                item = Car(item); # erstes Listenelement: var
                if (!symbolp(item)) goto fehler_symbol;
                if (constantp(TheSymbol(item))) goto fehler_constant;
                # Variable im STACK ablegen:
                pushSTACK(item); pushSTACK(Fixnum_0); opt_count++; var_count++;
                if (consp(item_rest))
                  { init_form = Car(item_rest); # zweites Listenelement: init
                    item_rest = Cdr(item_rest);
                    if (consp(item_rest))
                      { if (mconsp(Cdr(item_rest)))
                          # varspec ist zu lang
                          { pushSTACK(*(closure_ STACKop -1)); # ganze Lambda-Liste
                            fehler(source_program_error,
                                   GETTEXT("FUNCTION: too long variable specification after &OPTIONAL: ~")
                                  );
                          }
                        item = Car(item_rest); # drittes Listenelement: svar
                        if (!symbolp(item)) goto fehler_symbol;
                        if (constantp(TheSymbol(item))) goto fehler_constant;
                        # svar-Bit für var setzen:
                        STACK_0 = fixnum_inc(STACK_0,bit(svar_bit));
                        # Variable im STACK ablegen:
                        pushSTACK(item); pushSTACK(Fixnum_0); var_count++;
                  }   }
                  else
                  { init_form = NIL; } # Default-Init
              }
            # init_form vor (clos_opt_inits closure) pushen:
            pushSTACK(lambdalist); pushSTACK(init_form);
            { var object new_cons = allocate_cons();
              Car(new_cons) = popSTACK();
             {var object closure = *closure_;
              Cdr(new_cons) = TheIclosure(closure)->clos_opt_inits;
              TheIclosure(closure)->clos_opt_inits = new_cons;
            }}
            lambdalist = popSTACK();
          }}
        rest: # &REST-Parameter abarbeiten und auf dem Stack ablegen:
        { NEXT_ITEM(badrest,badrest,badrest,badrest,badrest,badrest);
          if (!symbolp(item)) goto fehler_symbol;
          if (constantp(TheSymbol(item))) goto fehler_constant;
          # Variable im STACK ablegen:
          pushSTACK(item); pushSTACK(Fixnum_0); var_count++;
          # Rest-Flag auf T setzen:
          TheIclosure(*closure_)->clos_rest_flag = T;
        }
        { NEXT_ITEM(badLLkey,badLLkey,key,badLLkey,aux,ende);
          pushSTACK(*(closure_ STACKop -1)); # ganze Lambda-Liste
          fehler(source_program_error,
                 GETTEXT("FUNCTION: &REST var must be followed by &KEY or &AUX or end of list: ~")
                );
        }
        badrest:
          pushSTACK(*(closure_ STACKop -1)); # ganze Lambda-Liste
          fehler(source_program_error,
                 GETTEXT("FUNCTION: &REST must be followed by a variable: ~")
                );
        key: # &KEY-Parameter abarbeiten, auf dem STACK ablegen
             # und Init-Formen in die Closure stecken:
        TheIclosure(*closure_)->clos_keywords = NIL; # keywords:=NIL
        loop
          { NEXT_ITEM(badLLkey,badLLkey,badLLkey,allow,aux,ende);
           {var object keyword;
            var object init_form;
            # Parse Variablenspezifikation in item:
            #   var  oder  (var [init [svar]])  oder ((key var) [init [svar]])
            # Lege var und evtl. svar auf den STACK, setze in var evtl.
            # das svar_bit. Liefert auch das Keyword in keyword und
            # init (oder NIL) in init_form.
            check_STACK();
            if (atomp(item))
              { if (!symbolp(item)) goto fehler_symbol;
                if (constantp(TheSymbol(item))) goto fehler_constant;
                # Variable im STACK ablegen:
                pushSTACK(item); pushSTACK(Fixnum_0); key_count++; var_count++;
                # Keyword holen:
                pushSTACK(lambdalist);
                keyword = intern_keyword(Symbol_name(item));
                lambdalist = popSTACK();
                # Default-Init:
                init_form = NIL;
              }
              else
              { var object item_rest = Cdr(item); # ([init [svar]])
                item = Car(item); # erstes Listenelement: var oder (key var)
                if (atomp(item))
                  # item = var
                  { if (!symbolp(item)) goto fehler_symbol;
                    if (constantp(TheSymbol(item))) goto fehler_constant;
                    # Variable im STACK ablegen:
                    pushSTACK(item); pushSTACK(Fixnum_0); key_count++; var_count++;
                    # Keyword holen:
                    pushSTACK(item_rest); pushSTACK(lambdalist);
                    keyword = intern_keyword(Symbol_name(item));
                    lambdalist = popSTACK(); item_rest = popSTACK();
                  }
                  else
                  # item = (key var)
                  { keyword = Car(item); # key
                    # sollte ein Symbol (früher: Keyword) sein:
                    if (!symbolp(keyword))
                      { pushSTACK(*(closure_ STACKop -1)); # ganze Lambda-Liste
                        pushSTACK(keyword);
                        fehler(source_program_error,
                               GETTEXT("FUNCTION: ~ in ~ is not a symbol")
                              );
                      }
                    item = Cdr(item); # (var)
                    if (!(consp(item) && matomp(Cdr(item))))
                      goto fehler_keyspec;
                    item = Car(item); # var
                    if (!symbolp(item)) goto fehler_symbol;
                    if (constantp(TheSymbol(item))) goto fehler_constant;
                    # Variable im STACK ablegen:
                    pushSTACK(item); pushSTACK(Fixnum_0); key_count++; var_count++;
                  }
                if (consp(item_rest))
                  { init_form = Car(item_rest); # zweites Listenelement: init
                    item_rest = Cdr(item_rest); # ([svar])
                    if (consp(item_rest))
                      { if (mconsp(Cdr(item_rest))) goto fehler_keyspec;
                        item = Car(item_rest); # drittes Listenelement: svar
                        if (!symbolp(item)) goto fehler_symbol;
                        if (constantp(TheSymbol(item))) goto fehler_constant;
                        # svar-Bit in var setzen:
                        STACK_0 = fixnum_inc(STACK_0,bit(svar_bit));
                        # Variable im STACK ablegen:
                        pushSTACK(item); pushSTACK(Fixnum_0); var_count++;
                  }   }
                  else
                  { init_form = NIL; } # Default-Init
              }
            # keyword vor (clos_keywords closure) pushen und
            # init_form vor (clos_key_inits closure) pushen:
            pushSTACK(lambdalist); pushSTACK(init_form); pushSTACK(keyword);
            { var object new_cons = allocate_cons();
              Car(new_cons) = popSTACK();
             {var object closure = *closure_;
              Cdr(new_cons) = TheIclosure(closure)->clos_keywords;
              TheIclosure(closure)->clos_keywords = new_cons;
            }}
            { var object new_cons = allocate_cons();
              Car(new_cons) = popSTACK();
             {var object closure = *closure_;
              Cdr(new_cons) = TheIclosure(closure)->clos_key_inits;
              TheIclosure(closure)->clos_key_inits = new_cons;
            }}
            lambdalist = popSTACK();
          }}
        fehler_keyspec:
          pushSTACK(*(closure_ STACKop -1)); # ganze Lambda-Liste
          fehler(source_program_error,
                 GETTEXT("FUNCTION: incorrect variable specification after &KEY: ~")
                );
        allow: # &ALLOW-OTHER-KEYS abarbeiten:
        { TheIclosure(*closure_)->clos_allow_flag = T; # Flag auf T setzen
          NEXT_ITEM(badLLkey,badLLkey,badLLkey,badLLkey,aux,ende);
          pushSTACK(*(closure_ STACKop -1)); # ganze Lambda-Liste
          fehler(source_program_error,
                 GETTEXT("FUNCTION: &ALLOW-OTHER-KEYS must be followed by &AUX or end of list: ~")
                );
        }
        aux: # &AUX-Parameter abarbeiten, auf dem STACK ablegen und
             # Init-Formen in die Closure stecken:
        loop
          { NEXT_ITEM(badLLkey,badLLkey,badLLkey,badLLkey,badLLkey,ende);
           {var object init_form;
            # Parse Variablenspezifikation in item:
            #   var  oder  (var [init])
            # Lege var auf den STACK.
            # Liefert auch init (oder NIL) in init_form.
            check_STACK();
            if (atomp(item))
              { if (!symbolp(item)) goto fehler_symbol;
                if (constantp(TheSymbol(item))) goto fehler_constant;
                # Variable im STACK ablegen:
                pushSTACK(item); pushSTACK(Fixnum_0); aux_count++; var_count++;
                init_form = NIL; # Default-Init
              }
              else
              { var object item_rest = Cdr(item);
                item = Car(item); # erstes Listenelement: var
                if (!symbolp(item)) goto fehler_symbol;
                if (constantp(TheSymbol(item))) goto fehler_constant;
                # Variable im STACK ablegen:
                pushSTACK(item); pushSTACK(Fixnum_0); aux_count++; var_count++;
                if (consp(item_rest))
                  { init_form = Car(item_rest); # zweites Listenelement: init
                    if (mconsp(Cdr(item_rest)))
                      # varspec ist zu lang
                      { pushSTACK(*(closure_ STACKop -1)); # ganze Lambda-Liste
                        fehler(source_program_error,
                               GETTEXT("FUNCTION: too long variable specification after &AUX: ~")
                              );
                  }   }
                  else
                  { init_form = NIL; } # Default-Init
              }
            # init_form vor (clos_aux_inits closure) pushen:
            pushSTACK(lambdalist); pushSTACK(init_form);
            { var object new_cons = allocate_cons();
              Car(new_cons) = popSTACK();
             {var object closure = *closure_;
              Cdr(new_cons) = TheIclosure(closure)->clos_aux_inits;
              TheIclosure(closure)->clos_aux_inits = new_cons;
            }}
            lambdalist = popSTACK();
          }}
        # Gesammelte Fehlermeldungen:
        badLLkey:
          pushSTACK(*(closure_ STACKop -1)); # ganze Lambda-Liste
          pushSTACK(item);
          fehler(source_program_error,
                 GETTEXT("FUNCTION: badly placed lambda-list keyword ~: ~")
                );
        fehler_symbol:
          pushSTACK(item);
          fehler(source_program_error,
                 GETTEXT("FUNCTION: ~ is not a symbol, may not be used as a variable")
                );
        fehler_constant:
          pushSTACK(item);
          fehler(program_error,
                 GETTEXT("FUNCTION: ~ is a constant, may not be used as a variable")
                );
        ende: # Listenende erreicht
        #undef NEXT_ITEM
        if (((uintL)~(uintL)0 > lp_limit_1) && (var_count > lp_limit_1)) # Zu viele Parameter?
          { pushSTACK(*(closure_ STACKop -1)); # ganze Lambda-Liste
            fehler(source_program_error,
                   GETTEXT("FUNCTION: too many parameters in the lambda-list ~")
                  );
          }
        # Da nun var_count <= lp_limit_1, passen alle counts in ein uintC.
        if (!nullp(lambdalist)) # Lambda-Liste eine Dotted List?
          { pushSTACK(*(closure_ STACKop -1)); # ganze Lambda-Liste
            fehler(source_program_error,
                   GETTEXT("FUNCTION: a dot in a lambda-list is allowed only for macros, not here: ~")
                  );
          }
        # Variablen zu einem Vektor zusammenfassen und in die Closure,
        # Variablenflags zu einem Byte-Vektor zusammenfassen und in die Closure:
        pushSTACK(allocate_bit_vector(intBsize*(var_count-spec_count))); # Byte-Vektor erzeugen
        { var object vars = allocate_vector(var_count); # Vektor erzeugen
          var object varflags = popSTACK();
          # Variablen in den Vektor schreiben (letzte hinten, erste vorne):
          { var object* ptr = &TheSvector(vars)->data[var_count];
            var uintB* ptrflags = &TheSbvector(varflags)->data[var_count-spec_count];
            var uintC count;
            dotimesC(count,var_count-spec_count,
              { *--ptrflags = (uintB)posfixnum_to_L(popSTACK());
                *--ptr = popSTACK();
              });
            dotimesC(count,spec_count, { *--ptr = popSTACK(); } );
          }
         {var object closure = *closure_;
          TheIclosure(closure)->clos_vars     = vars;
          TheIclosure(closure)->clos_varflags = varflags;
        # Anzahlen in die Closure eintragen:
          TheIclosure(closure)->clos_spec_anz = fixnum(spec_count);
          TheIclosure(closure)->clos_req_anz  = fixnum(req_count);
          TheIclosure(closure)->clos_opt_anz  = fixnum(opt_count);
          TheIclosure(closure)->clos_key_anz  = fixnum(key_count);
          TheIclosure(closure)->clos_aux_anz  = fixnum(aux_count);
        # Im Variablen-Vektor sind die ersten spec_count Variablen die
        # SPECIAL-Deklarierten. In jeder übrigen Variablen wird das DYNAM_BIT
        # gesetzt, falls sie unter den SPECIAL-deklarierten vorkommt.
          if (!(spec_count==0))
            { # Schleife über die übrigen Variablen:
              if (var_count-spec_count > 0)
                { var object* othervarptr = &TheSvector(vars)->data[spec_count];
                  var uintB* othervarflagsptr = &TheSbvector(varflags)->data[0];
                  var uintC count1;
                  dotimespC(count1,var_count-spec_count,
                    { var object othervar = *othervarptr++; # nächste Variable
                      # Suche sie in den SPECIAL-deklarierten Variablen:
                      {var object* specvarptr = &TheSvector(vars)->data[0];
                       var uintC count2;
                       dotimespC(count2,spec_count,
                         { if (eq(*specvarptr++,othervar)) # gefunden?
                             # ja -> also ist die Variable othervar dynamisch zu binden.
                             { *othervarflagsptr |= bit(dynam_bit); break; }
                         });
                      }
                      othervarflagsptr++;
                    });
            }   }
        # Schließlich noch die akkumulierten Listen in der Closure umdrehen:
          nreverse(TheIclosure(closure)->clos_opt_inits);
          nreverse(TheIclosure(closure)->clos_keywords);
          nreverse(TheIclosure(closure)->clos_key_inits);
          nreverse(TheIclosure(closure)->clos_aux_inits);
        # Fertig.
        # Stackaufbau: closure, lambdalist.
          skipSTACK(2);
          return closure;
        }}
    }}}}

# Fehler, wenn aufzurufendes Symbol eine Spezialform ist.
# fehler_specialform(caller,funname);
# > caller: Aufrufer (ein Symbol)
# > funname: ein Symbol
  nonreturning_function(local, fehler_specialform, (object caller, object funname));
  local void fehler_specialform(caller,funname)
    var object caller;
    var object funname;
    { pushSTACK(funname); # Wert für Slot NAME von CELL-ERROR
      pushSTACK(funname);
      pushSTACK(caller);
      fehler(undefined_function,
             GETTEXT("~: ~ is a special form, not a function")
            );
    }

# Fehler, wenn aufzurufendes Symbol ein Macro ist.
# fehler_macro(caller,funname);
# > caller: Aufrufer (ein Symbol)
# > funname: ein Symbol
  nonreturning_function(local, fehler_macro, (object caller, object funname));
  local void fehler_macro(caller,funname)
    var object caller;
    var object funname;
    { pushSTACK(funname); # Wert für Slot NAME von CELL-ERROR
      pushSTACK(funname);
      pushSTACK(caller);
      fehler(undefined_function,
             GETTEXT("~: ~ is a macro, not a function")
            );
    }

# Fehlermeldung wegen undefinierter Funktion.
# fehler_undefined(caller,funname);
# > caller: Aufrufer (ein Symbol)
# > funname: Symbol oder (SETF symbol)
  nonreturning_function(local, fehler_undefined, (object caller, object funname));
  local void fehler_undefined(caller,funname)
    var object caller;
    var object funname;
    { pushSTACK(funname); # Wert für Slot NAME von CELL-ERROR
      pushSTACK(funname);
      pushSTACK(caller);
      fehler(undefined_function,
             GETTEXT("~: the function ~ is undefined")
            );
    }

# UP: Wandelt ein Argument in eine Funktion um.
# coerce_function(obj)
# > obj: Objekt
# > subr_self: Aufrufer (ein SUBR)
# < ergebnis: Objekt als Funktion (SUBR oder Closure)
# can trigger GC
  global object coerce_function (object obj);
  global object coerce_function(obj)
    var object obj;
    { # obj sollte ein Symbol, ein SUBR oder eine Closure sein.
      if (subrp(obj)) { return obj; } # SUBR ist OK
      elif (closurep(obj)) { return obj; } # Closure ist OK
      #ifdef DYNAMIC_FFI
      elif (ffunctionp(obj)) { return obj; } # Foreign-Function ist OK
      #endif
      elif (symbolp(obj))
        { var object fdef = Symbol_function(obj);
          if (subrp(fdef)) { return fdef; }
          elif (closurep(fdef)) { return fdef; }
          #ifdef DYNAMIC_FFI
          elif (ffunctionp(fdef)) { return fdef; }
          #endif
          elif (orecordp(fdef))
            { fehler_specialform(TheSubr(subr_self)->name,obj); }
          elif (consp(fdef))
            { fehler_macro(TheSubr(subr_self)->name,obj); }
          else
            { fehler_undefined(TheSubr(subr_self)->name,obj); }
        }
      elif (funnamep(obj))
        { var object symbol = get(Car(Cdr(obj)),S(setf_function)); # (get ... 'SYS::SETF-FUNCTION)
          if (!symbolp(symbol)) # sollte (uninterniertes) Symbol sein
            { fehler_undefined(TheSubr(subr_self)->name,obj); }
         {var object fdef = Symbol_function(symbol);
          if (subrp(fdef)) { return fdef; }
          elif (closurep(fdef)) { return fdef; }
          #ifdef DYNAMIC_FFI
          elif (ffunctionp(fdef)) { return fdef; }
          #endif
          else
            { fehler_undefined(TheSubr(subr_self)->name,obj); }
        }}
      elif (consp(obj) && eq(Car(obj),S(lambda))) # Cons (LAMBDA . ...) ?
        { fehler_lambda_expression(obj); }
      else
        { pushSTACK(obj);
          pushSTACK(TheSubr(subr_self)->name);
          fehler(error,
                 GETTEXT("~: ~ is not a function")
                );
        }
    }

#ifdef DEBUG_EVAL

# Emit some trace output for a function call, to *funcall-trace-output*.
# trace_call(fun,type_of_call,caller_type);
# > object fun: function being called, a SUBR/FSUBR/Closure
# > uintB type_of_call: 'A' for apply, 'F' for funcall, 'B' for bytecode
# > uintB caller_type: 'F' for fsubr, 'S' for subr,
#                      'C' for cclosure, 'I' for iclosure
# can trigger GC
  local void trace_call (object fun, uintB type_of_call, uintB caller_type);
  local void trace_call(fun,type_of_call,caller_type)
    var object fun;
    var uintB type_of_call;
    var uintB caller_type;
    { var object stream = Symbol_value(S(funcall_trace_output)); # SYS::*FUNCALL-TRACE-OUTPUT*
      # No output until *funcall-trace-output* has been initialized:
      if (!streamp(stream)) return;
      pushSTACK(stream);
      if (cclosurep(fun))
        { pushSTACK(TheCclosure(fun)->clos_name);
          write_ascii_char(&STACK_1,'c');
        }
      elif (closurep(fun))
        { pushSTACK(TheClosure(fun)->clos_name);
          write_ascii_char(&STACK_1,'C');
        }
      elif (subrp(fun))
        { pushSTACK(TheSubr(fun)->name);
          write_ascii_char(&STACK_1,'S');
        }
      elif (fsubrp(fun))
        { pushSTACK(TheFsubr(fun)->name);
          write_ascii_char(&STACK_1,'F');
        }
      else
        { pushSTACK(NIL);
          write_ascii_char(&STACK_1,'?');
        }
      write_ascii_char(&STACK_1,type_of_call); # output type of call
      write_ascii_char(&STACK_1,caller_type);  # output caller
      write_ascii_char(&STACK_1,'[');
      prin1(&STACK_1,STACK_0);            # output function name
      write_ascii_char(&STACK_1,']');
      terpri(&STACK_1);
      skipSTACK(2);
    }

#endif

# Fehlermeldung bei unpaarigen Keyword-Argumenten
# fehler_key_unpaarig(fun);
# > fun: Funktion
  nonreturning_function(local, fehler_key_unpaarig, (object fun));
  local void fehler_key_unpaarig(fun)
    var object fun;
    { pushSTACK(fun);
      fehler(program_error,
             GETTEXT("EVAL/APPLY: keyword arguments for ~ should occur pairwise")
            );
    }

# Fehlermeldung bei zu vielen Keyword-Argumenten
# fehler_key_zuviel(fun);
# > fun: Funktion
  nonreturning_function(local, fehler_key_zuviel, (object fun));
  local void fehler_key_zuviel(fun)
    var object fun;
    { pushSTACK(fun);
      fehler(program_error,
             GETTEXT("EVAL/APPLY: too many arguments given to ~")
            );
    }

# Fehlermeldung bei fehlerhaftem Keyword
# fehler_key_notkw(kw);
# > kw: Nicht-Symbol
  nonreturning_function(local, fehler_key_notkw, (object kw));
  local void fehler_key_notkw(kw)
    var object kw;
    { pushSTACK(kw); # Wert für Slot DATUM von KEYWORD-ERROR
      pushSTACK(S(symbol)); # Wert für Slot EXPECTED-TYPE von KEYWORD-ERROR
      pushSTACK(kw);
      fehler(keyword_error,
             GETTEXT("EVAL/APPLY: ~ is not a symbol")
            );
    }

# Fehlermeldung bei fehlerhaftem Keyword
# fehler_key_badkw(fun,kw,kwlist);
# > fun: Funktion
# > kw: unzulässiges Keyword
# > kwlist: Liste der zugelassenen Keywords
  nonreturning_function(local, fehler_key_badkw, (object fun, object kw, object kwlist));
  local void fehler_key_badkw(fun,kw,kwlist)
    var object fun;
    var object kw;
    var object kwlist;
    { pushSTACK(kw); # Wert für Slot DATUM von KEYWORD-ERROR
      pushSTACK(kwlist);
      pushSTACK(kwlist);
      pushSTACK(fun);
      pushSTACK(kw);
      {var object type = allocate_cons();
       Car(type) = S(member); Cdr(type) = STACK_3;
       STACK_3 = type; # `(MEMBER ,@kwlist) = Wert für Slot EXPECTED-TYPE von KEYWORD-ERROR
      }
      fehler(keyword_error,
             GETTEXT("EVAL/APPLY: keyword ~ is illegal for ~. The possible keywords are ~")
            );
    }

# Test auf unerlaubte Keywords
# check_for_illegal_keywords(allow_flag,fehler_statement);
# > uintC argcount: Anzahl der Keyword/Value-Paare
# > object* rest_args_pointer: Pointer über die 2*argcount restlichen Argumente
# > boolean allow_flag: Flag, ob &ALLOW-OTHER-KEYS angegeben war
# > for_every_keyword: Macro, der alle Keywords durchläuft und an 'keyword'
#                      zuweist.
# > fehler_statement: Statement, das meldet, dass bad_keyword illegal ist.
  #define check_for_illegal_keywords(allow_flag_expr,fehler_statement)  \
    { var object* argptr = rest_args_pointer; # Pointer in die Argumente    \
      var object bad_keyword = nullobj; # erstes unerlaubtes Keyword oder nullobj \
      var boolean allow_flag = # Flag für allow-other-keys (ob              \
        # &ALLOW-OTHER-KEYS angegeben war oder ':ALLOW-OTHER-KEY T' vorkam) \
        (allow_flag_expr);                                                  \
      var uintC check_count;                                                \
      dotimesC(check_count,argcount,                                        \
        { var object kw = NEXT(argptr); # nächstes Argument                 \
          var object val = NEXT(argptr); # und Wert dazu                    \
          # muss ein Symbol, sollte ein Keyword sein:                       \
          if (!symbolp(kw))                                                 \
            { fehler_key_notkw(kw); }                                       \
          if (!allow_flag) # andere Keywords erlaubt? ja -> ok              \
            { if (eq(kw,S(Kallow_other_keys))) #  Kommt :ALLOW-OTHER-KEYS ? \
                { if (!nullp(val)) { allow_flag = TRUE; } }                 \
                else                                                        \
                # bis hierher war nicht :ALLOW-OTHER-KEYS da, und NOALLOW   \
                { if (eq(bad_keyword,nullobj)) # bisher alle Keywords ok?   \
                    # muss testen, ob das Keyword kw erlaubt ist.           \
                    { for_every_keyword(                                    \
                        { if (eq(keyword,kw)) goto kw_ok; }                 \
                        );                                                  \
                      # Keyword kw war nicht erlaubt.                       \
                      bad_keyword = kw;                                     \
                      kw_ok: ;                                              \
            }   }   }                                                       \
        });                                                                 \
      if (!allow_flag)                                                      \
        if (!eq(bad_keyword,nullobj))                                       \
          # falsches Keyword aufgetreten                                    \
          { fehler_statement }                                              \
    }

# Zu einem Keyword 'keyword' das Paar Key.Wert suchen:
# find_keyword_value( notfound_statement, found_statement );
# > keyword: Keyword
# > uintC argcount: Anzahl der Keyword/Value-Paare
# > object* rest_args_pointer: Pointer über die 2*argcount restlichen Argumente
# > notfound_statement: Was zu tun ist, wenn nicht gefunden
# > found_statement: Was zu tun ist, wenn Wert value gefunden
  #define find_keyword_value(notfound_statement,found_statement)  \
    { var object* argptr = rest_args_pointer;                               \
      var uintC find_count;                                                 \
      dotimesC(find_count,argcount,                                         \
        { if (eq(NEXT(argptr),keyword)) goto kw_found; # richtiges Keyword? \
          NEXT(argptr);                                                     \
        });                                                                 \
      if (TRUE)                                                             \
        # nicht gefunden                                                    \
        { notfound_statement }                                              \
        else                                                                \
        kw_found: # gefunden                                                \
        { var object value = NEXT(argptr);                                  \
          found_statement                                                   \
        }                                                                   \
    }

# UP: Wendet eine interpretierte Closure auf Argumente an.
# funcall_iclosure(closure,args_pointer,argcount);
# > closure: Closure
# > args_pointer: Pointer über die Argumente (im Stack)
# > argcount: Anzahl der Argumente
# < mv_count/mv_space: Werte
# < STACK: aufgeräumt, = args_pointer
# can trigger GC
  local Values funcall_iclosure (object closure, object* args_pointer, uintC argcount);
  local Values funcall_iclosure(closure,args_pointer,argcount)
    var object closure;
    var object* args_pointer;
    var uintC argcount;
    { # 1. Schritt: APPLY-Frame zu Ende aufbauen:
      var sp_jmp_buf my_jmp_buf;
      #ifdef DEBUG_EVAL
      if (streamp(Symbol_value(S(funcall_trace_output))))
        { pushSTACK(closure); trace_call(closure,'F','I'); closure = popSTACK(); }
      #endif
      { var object* top_of_frame = args_pointer; # Pointer übern Frame
        pushSTACK(closure);
        finish_entry_frame(APPLY,&!my_jmp_buf,,
          { if (mv_count==0) # nach Wiedereintritt: Form übergeben?
              { closure = STACK_(frame_closure); # selben APPLY nochmals versuchen
                args_pointer = topofframe(STACK_0);
                argcount = STACK_item_count(STACK STACKop frame_args,args_pointer);
              }
              else
              { setSTACK(STACK = topofframe(STACK_0)); # STACK aufräumen # oder unwind() ??
                eval_noenv(value1); return; # übergebene Form evaluieren
          }   }
          );
      }
     {var object* closure_ = &STACK_(frame_closure); # Pointer auf die Closure
      var object* frame_pointer; # Pointer in den Frame
      # 2. Schritt: Variablenbindungsframe aufbauen:
      { var object* top_of_frame = STACK; # Pointer übern Frame
        var object vars = TheIclosure(closure)->clos_vars; # Vektor mit Variablennamen
        var uintL var_count = Svector_length(vars); # Anzahl der Variablen
        get_space_on_STACK(var_count * 2 * sizeof(object)); # Platz reservieren
        { var object* varptr = &TheSvector(vars)->data[0]; # Pointer auf Variablen im Vektor
          var uintC spec_count = posfixnum_to_L(TheIclosure(closure)->clos_spec_anz);
          var uintC count;
          # erst die Special-Referenzen:
          dotimesC(count,spec_count,
            { # Bindung mit "Wert" specdecl:
              pushSTACK(specdecl);
              pushSTACK_symbolwithflags(*varptr++,wbit(active_bit_o)); # Bindung schon mal als aktiv vormerken
            });
          frame_pointer = args_end_pointer;
          if (var_count-spec_count > 0)
            { var uintB* varflagsptr = &TheSbvector(TheIclosure(closure)->clos_varflags)->data[0];
              dotimespC(count,var_count-spec_count,
                { pushSTACK(NIL); # NIL als vorläufiger Wert
                 {var object next_var = *varptr++; # nächste Variable
                  var oint next_varflags = (oint)(*varflagsptr++)<<oint_symbolflags_shift; # mit evtl. dynam_bit, svar_bit
                  if (special_var_p(TheSymbol(next_var))) # SPECIAL-proklamiert?
                    { next_varflags |= wbit(dynam_bit_o); } # -> dynamisch binden
                  pushSTACK_symbolwithflags(next_var,next_varflags);
                }});
        }   }
        # VAR_ENV der Closure wird NEXT_ENV im Frame:
        pushSTACK(TheIclosure(closure)->clos_var_env);
        pushSTACK(as_object(var_count)); # var_count Bindungen, alle noch ungenestet
        finish_frame(VAR);
      }
      # STACK zeigt nun unter den Variablenbindungs-Frame.
      # frame_pointer = Pointer in den Variablenbindungsframe, über die erste
      # noch inaktive Bindung, unter die bereits aktiven SPECIAL-Referenzen.
      {var object new_var_env = make_framepointer(STACK);
       # Dieser Frame wird nachher zum neuen VAR_ENV.
      # 3. Schritt: aktuelle Environments binden:
       make_ENV5_frame();
      # Das Closure-Environment aktivieren:
       aktenv.var_env   = new_var_env; # Variablenbindungsframe
       aktenv.fun_env   = TheIclosure(closure)->clos_fun_env;
       aktenv.block_env = TheIclosure(closure)->clos_block_env;
       aktenv.go_env    = TheIclosure(closure)->clos_go_env;
       aktenv.decl_env  = TheIclosure(closure)->clos_decl_env;
      }
      # Stackaufbau:
      #   APPLY-Frame
      #   Variablenbindungsframe
      #   ENV-Frame
      # 4. Schritt: Parameter abarbeiten:
      { check_SP();
        # Macro zum Binden von Variablen im Variablenframe:
        # Bindet die nächste Variable an value, erniedrigt frame_pointer um 2 bzw. 3.
        # (Benutzt, dass varframe_binding_mark = 0 !)
        #define bind_next_var(value,markptr_zuweisung)  \
          { frame_pointer skipSTACKop -varframe_binding_size;                                  \
           {var object* markptr = markptr_zuweisung &Before(frame_pointer);                    \
            if (as_oint(*markptr) & wbit(dynam_bit_o))                                         \
              # dynamische Bindung aktivieren:                                                 \
              { var object sym = *(markptr STACKop varframe_binding_sym); # Variable           \
                *(markptr STACKop varframe_binding_value) = TheSymbolflagged(sym)->symvalue; # alten Wert in den Frame \
                *markptr = as_object(as_oint(*markptr) | wbit(active_bit_o)); # Bindung aktivieren \
                TheSymbolflagged(sym)->symvalue = (value); # neuen Wert in die Wertzelle       \
              }                                                                                \
              else                                                                             \
              # statische Bindung aktivieren:                                                  \
              { *(markptr STACKop varframe_binding_value) = (value); # neuen Wert in den Frame \
                *markptr = as_object(as_oint(*markptr) | wbit(active_bit_o)); # Bindung aktivieren \
              }                                                                                \
          }}
        # required-Parameter abarbeiten:
        # Es ist das jeweils nächste Argument zu holen und im Stack zu binden.
        { var uintC count = posfixnum_to_L(TheIclosure(closure)->clos_req_anz);
          if (count>0)
            { if (argcount < count)
                { pushSTACK(TheIclosure(closure)->clos_name);
                  fehler(program_error,
                         GETTEXT("EVAL/APPLY: too few arguments given to ~")
                        );
                }
              argcount -= count;
              dotimespC(count,count,
                { var object next_arg = NEXT(args_pointer); # nächstes Argument
                  bind_next_var(next_arg,); # nächste Variable binden
                });
        }   }
        # optionale Parameter abarbeiten:
        # Es ist jeweils das nächste Argument zu holen; falls keines vorliegt,
        # eine Init-Form auszuführen; dann im Stack zu binden.
        { var uintC count = posfixnum_to_L(TheIclosure(closure)->clos_opt_anz);
          if (count==0) goto optional_ende;
         {var object inits = TheIclosure(closure)->clos_opt_inits; # Init-Formen
          do { if (argcount==0) goto optional_aus;
               argcount--;
              {var object next_arg = NEXT(args_pointer); # nächstes Argument
               {var object* optmarkptr;
                bind_next_var(next_arg,optmarkptr=); # nächste Variable binden
                if (as_oint(*optmarkptr) & wbit(svar_bit_o)) # supplied-p-Parameter folgt?
                  { *optmarkptr = as_object(as_oint(*optmarkptr) & ~wbit(svar_bit_o));
                    bind_next_var(T,); # ja -> an T binden
               }  }
               inits = Cdr(inits); # Init-Formen-Liste verkürzen
               count--;
             }}
             until (count==0);
          goto optional_ende;
          optional_aus: # Hier sind die optionalen Argumente ausgegangen.
          pushSTACK(inits);
         }# Ab hier alle Init-Formen der optionalen Parameter ausführen:
          dotimespC(count,count,
            { var object inits = STACK_0; # restliche Initformen
              STACK_0 = Cdr(inits);
              inits = (eval(Car(inits)),value1); # nächste Initform, ausgewertet
             {var object* optmarkptr;
              bind_next_var(inits,optmarkptr=); # nächste Variable binden
              if (as_oint(*optmarkptr) & wbit(svar_bit_o)) # supplied-p-Parameter folgt?
                { *optmarkptr = as_object(as_oint(*optmarkptr) & ~wbit(svar_bit_o));
                  bind_next_var(NIL,); # ja -> an NIL binden
             }  }
            });
          closure = *closure_;
          # &REST-Parameter ohne Argumente initialisieren:
          if (!nullp(TheIclosure(closure)->clos_rest_flag)) # Rest-Flag?
            { bind_next_var(NIL,); } # ja -> an NIL binden
          # &KEY-Parameter ohne Argumente initialisieren:
          count = posfixnum_to_L(TheIclosure(closure)->clos_key_anz); # Anzahl Keyword-Parameter
          if (count>0)
            { STACK_0 = TheIclosure(closure)->clos_key_inits; # zugehörige Init-Formen
              dotimespC(count,count,
                { var object inits = STACK_0; # restliche Initformen
                  STACK_0 = Cdr(inits);
                  inits = (eval(Car(inits)),value1); # nächste Initform, ausgewertet
                 {var object* keymarkptr;
                  bind_next_var(inits,keymarkptr=); # nächste Variable binden
                  if (as_oint(*keymarkptr) & wbit(svar_bit_o)) # supplied-p-Parameter folgt?
                    { *keymarkptr = as_object(as_oint(*keymarkptr) & ~wbit(svar_bit_o));
                      bind_next_var(NIL,); # ja -> an NIL binden
                 }  }
                });
              closure = *closure_;
            }
          skipSTACK(1); # restliche Init-Formen vergessen
          goto aux; # weiter zu den AUX-Variablen
        }
        optional_ende:
        # &KEY-Parameter und &REST-Parameter vorbereiten:
        if (numberp(TheIclosure(closure)->clos_keywords) # keyword eine Zahl?
            && nullp(TheIclosure(closure)->clos_rest_flag) # und kein Rest-Parameter?
           )
          # ja -> weder &KEY noch &REST angegeben
          { if (argcount>0) # noch Argumente da -> Fehler
              { pushSTACK(TheIclosure(closure)->clos_name);
                fehler(program_error,
                       GETTEXT("EVAL/APPLY: too many arguments given to ~")
                      );
              }
          }
          else
          # &KEY oder &REST vorhanden.
          { # &REST-Parameter abarbeiten:
            if (!nullp(TheIclosure(closure)->clos_rest_flag)) # Rest-Parameter vorhanden?
              # ja -> übrige Argumente zu einer Liste zusammenfassen:
              { pushSTACK(NIL); # Listenanfang
                if (argcount>0)
                  {var object* ptr = args_pointer STACKop -(uintP)argcount;
                   var uintC count;
                   dotimespC(count,argcount,
                     { var object new_cons = allocate_cons();
                       Car(new_cons) = BEFORE(ptr);
                       Cdr(new_cons) = STACK_0;
                       STACK_0 = new_cons;
                     });
                    closure = *closure_;
                  }
               {var object list = popSTACK(); # Gesamtliste
                bind_next_var(list,); # &REST-Parameter an diese Liste binden
              }}
            # &KEY-Parameter abarbeiten:
            if (!numberp(TheIclosure(closure)->clos_keywords))
              # Keyword-Parameter vorhanden
              { var object* rest_args_pointer = args_pointer;
                # argcount = Anzahl restlicher Argumente
                # argcount halbieren, gibt die Anzahl der Paare Key.Wert:
                if (!((argcount%2)==0))
                  # Anzahl war ungerade -> nicht paarig:
                  { fehler_key_unpaarig(TheIclosure(closure)->clos_name); }
                argcount = argcount/2;
                # Test auf unerlaubte Keywords:
                { var object keywords = TheIclosure(closure)->clos_keywords;
                  #define for_every_keyword(statement)         \
                    { var object keywordsr = keywords;         \
                      while (consp(keywordsr))                 \
                        { var object keyword = Car(keywordsr); \
                          statement;                           \
                          keywordsr = Cdr(keywordsr);          \
                    }   }
                  check_for_illegal_keywords(
                    !nullp(TheIclosure(closure)->clos_allow_flag),
                    { fehler_key_badkw(TheIclosure(closure)->clos_name,
                                       bad_keyword,
                                       TheIclosure(closure)->clos_keywords);
                    }
                    );
                  #undef for_every_keyword
                # Jetzt die Key-Werte zuordnen und die Key-Inits auswerten:
                 {var uintC count = posfixnum_to_L(TheIclosure(closure)->clos_key_anz);
                  if (count > 0)
                    { var object key_inits = TheIclosure(closure)->clos_key_inits;
                      dotimespC(count,count,
                        { var object keyword = Car(keywords); # Keyword
                          var object var_value;
                          var object svar_value;
                          # Zu diesem Keyword das Paar Key.Wert suchen:
                          find_keyword_value(
                            # nicht gefunden, muss den Init auswerten:
                            { pushSTACK(keywords); pushSTACK(key_inits);
                              var_value = (eval(Car(key_inits)),value1);
                              key_inits = popSTACK(); keywords = popSTACK();
                              svar_value = NIL; # NIL für evtl. supplied-p-Parameter
                            },
                            # gefunden -> Wert nehmen:
                            { var_value = value;
                              svar_value = T; # T für evtl. supplied-p-Parameter
                            }
                            );
                          {var object* keymarkptr;
                           bind_next_var(var_value,keymarkptr=); # Keyword-Variable binden
                           if (as_oint(*keymarkptr) & wbit(svar_bit_o)) # supplied-p-Parameter folgt?
                             { *keymarkptr = as_object(as_oint(*keymarkptr) & ~wbit(svar_bit_o));
                               bind_next_var(svar_value,); # ja -> an NIL bzw. T binden
                          }  }
                          keywords = Cdr(keywords);
                          key_inits = Cdr(key_inits);
                        });
                }}  }
                closure = *closure_;
          }   }
        aux: # &AUX-Parameter behandeln:
        { var uintC count = posfixnum_to_L(TheIclosure(closure)->clos_aux_anz);
          if (count>0)
            { pushSTACK(TheIclosure(closure)->clos_aux_inits); # Init-Formen für &AUX-Variablen
              dotimespC(count,count,
                { var object inits = STACK_0;
                  STACK_0 = Cdr(inits);
                  inits = (eval(Car(inits)),value1); # nächstes Init auswerten
                  bind_next_var(inits,); # und Variable daran binden
                });
              skipSTACK(1); # restliche Init-Formen vergessen
              closure = *closure_;
        }   }
        #undef bind_next_var
      }
      # 5. Schritt: Body auswerten:
      implicit_progn(TheIclosure(closure)->clos_body,NIL);
      unwind(); # ENV-Frame auflösen
      unwind(); # Variablenbindungsframe auflösen
      unwind(); # APPLY-Frame auflösen
      # fertig
    }}

# UP: Besorgt die Zuordnung der Key-Argumente bei SUBRs.
# Nur aufzurufen, falls key_flag /= subr_nokey.
# > fun: Funktion, ein SUBR
# > argcount: Argumentezahl nach den optionalen
# > STACK_(argcount-1),...,STACK_0: die argcount Argumente nach den optionalen
# > key_args_pointer: Pointer über die Key-Parameter im STACK
# > rest_args_pointer: Pointer über die restlichen Argumente im STACK
# < STACK: korrekt gesetzt
# verändert STACK
  local void match_subr_key (object fun, uintL argcount, object* key_args_pointer, object* rest_args_pointer);
  local void match_subr_key(fun,argcount,key_args_pointer,rest_args_pointer)
    var object fun;
    var uintL argcount;
    var object* key_args_pointer;
    var object* rest_args_pointer;
    { # argcount halbieren, gibt die Anzahl der Paare Key.Wert:
      if (!((argcount%2)==0))
        # Anzahl war ungerade -> nicht paarig:
        { fehler_key_unpaarig(fun); }
      if (((uintL)~(uintL)0 > ca_limit_1) && (argcount > ca_limit_1))
        { fehler_key_zuviel(fun); }
      # Da nun argcount <= ca_limit_1, passen alle count's in ein uintC.
      argcount = argcount/2;
      # Test auf unerlaubte Keywords:
      { var object* keywords_pointer = &TheSvector(TheSubr(fun)->keywords)->data[0];
        var uintC key_anz = TheSubr(fun)->key_anz;
        #define for_every_keyword(statement)  \
          if (key_anz > 0)                               \
            { var object* keywordptr = keywords_pointer; \
              var uintC count;                           \
              dotimespC(count,key_anz,                   \
                { var object keyword = *keywordptr++;    \
                  statement;                             \
                });                                      \
            }
        check_for_illegal_keywords(
          TheSubr(fun)->key_flag == subr_key_allow,
          { pushSTACK(bad_keyword); # fehlerhaftes Keyword retten
            # Keyword-Vektor in eine Liste umwandeln:
            # (SYS::COERCE-SEQUENCE kwvec 'LIST)
            coerce_sequence(TheSubr(fun)->keywords,S(list));
           {var object kwlist = value1;
            fehler_key_badkw(fun,popSTACK(),kwlist);
          }}
          );
        #undef for_every_keyword
      # Jetzt Argumente und Parameter zuordnen:
        if (key_anz > 0)
          { var object* keywordptr = keywords_pointer;
            var object* key_args_ptr = key_args_pointer;
            var uintC count;
            dotimespC(count,key_anz,
              { var object keyword = *keywordptr++; # Keyword
                # Zu diesem Keyword das Paar Key.Wert suchen:
                find_keyword_value(
                  # nicht gefunden -> Wert bleibt #<UNBOUND> :
                  { NEXT(key_args_ptr); },
                  # gefunden -> Wert eintragen:
                  { NEXT(key_args_ptr) = value; }
                  );
              });
      }   }
      # evtl. Rest-Parameter versorgen:
      if (TheSubr(fun)->rest_flag == subr_norest)
        # SUBR ohne &REST-Flag: restliche Argumente vergessen:
        { set_args_end_pointer(rest_args_pointer); }
        # SUBR mit &REST-Flag: restliche Argumente im Stack belassen
    }

# UP: Besorgt die Zuordnung zwischen Argumentliste und Keyword-Parametern
# und eventuellem Rest-Parameter einer compilierten Closure.
# > closure: compilierte Closure mit &KEY-Parametern
# > argcount: Argumentezahl nach den optionalen
# > STACK_(argcount-1),...,STACK_0: die argcount Argumente nach den optionalen
# > key_args_pointer: Pointer über die Key-Parameter im STACK
#                     (evtl. auch Pointer unter den Rest-Parameter im STACK,
#                      der = #<UNBOUND> ist, falls er noch zu versorgen ist)
# > rest_args_pointer: Pointer über die restlichen Argumente im STACK
# < STACK: korrekt gesetzt
# < ergebnis: closure
# verändert STACK
# can trigger GC
  local object match_cclosure_key (object closure, uintL argcount, object* key_args_pointer, object* rest_args_pointer);
  local object match_cclosure_key(closure,argcount,key_args_pointer,rest_args_pointer)
    var object closure;
    var uintL argcount;
    var object* key_args_pointer;
    var object* rest_args_pointer;
    { # argcount halbieren, gibt die Anzahl der Paare Key.Wert:
      if (!((argcount%2)==0))
        # Anzahl war ungerade -> nicht paarig:
        { fehler_key_unpaarig(closure); }
      if (((uintL)~(uintL)0 > ca_limit_1) && (argcount > ca_limit_1))
        { fehler_key_zuviel(closure); }
      # Da nun argcount <= ca_limit_1, passen alle count's in ein uintC.
      argcount = argcount/2;
     {var object codevec = TheCclosure(closure)->clos_codevec; # Code-Vektor
      {var uintC key_anz = TheCodevec(codevec)->ccv_numkey; # Anzahl Keywords
       var uintL keywords_offset = TheCodevec(codevec)->ccv_keyconsts; # Offset der Keywords in FUNC
       var object* keywords_pointer = # zeigt aufs erste Keyword
         (TheCodevec(codevec)->ccv_flags & bit(4) # generische Funktion?
          ? &TheSvector(TheCclosure(closure)->clos_consts[0])->data[keywords_offset]
          : &TheCclosure(closure)->clos_consts[keywords_offset]
         );
      # Test auf unerlaubte Keywords:
        #define for_every_keyword(statement)  \
          if (key_anz > 0)                               \
            { var object* keywordptr = keywords_pointer; \
              var uintC count;                           \
              dotimespC(count,key_anz,                   \
                { var object keyword = *keywordptr++;    \
                  statement;                             \
                });                                      \
            }
        check_for_illegal_keywords(
          !((TheCodevec(codevec)->ccv_flags & bit(6)) == 0),
          { pushSTACK(bad_keyword); # retten
            # Liste der erlaubten Keywords bilden:
            for_every_keyword( { pushSTACK(keyword); } );
           {var object kwlist = listof(key_anz);
            bad_keyword = popSTACK();
            # und Fehler melden:
            fehler_key_badkw(closure,bad_keyword,kwlist);
          }}
          );
        #undef for_every_keyword
      # Jetzt Argumente und Parameter zuordnen:
        if (key_anz > 0)
          { var object* keywordptr = keywords_pointer;
            var object* key_args_ptr = key_args_pointer;
            var uintC count;
            dotimespC(count,key_anz,
              { var object keyword = *keywordptr++; # Keyword
                # Zu diesem Keyword das Paar Key.Wert suchen:
                find_keyword_value(
                  # nicht gefunden -> Wert bleibt #<UNBOUND> :
                  { NEXT(key_args_ptr); },
                  # gefunden -> Wert eintragen:
                  { NEXT(key_args_ptr) = value; }
                  );
              });
      }   }
      # evtl. Rest-Parameter versorgen:
      if (TheCodevec(codevec)->ccv_flags & bit(0)) # Rest-Flag?
        # Closure mit Keywords und &REST-Flag:
        { var object* rest_arg_ = &BEFORE(key_args_pointer); # Pointer auf den REST-Parameter
          if (eq(*rest_arg_,unbound))
            # muss noch gefüllt werden: Liste basteln
            { *rest_arg_ = closure; # Closure retten
             {var object rest_arg = NIL;
              until (args_end_pointer == rest_args_pointer)
                { pushSTACK(rest_arg);
                  rest_arg = allocate_cons();
                  Cdr(rest_arg) = popSTACK();
                  Car(rest_arg) = popSTACK();
                }
              closure = *rest_arg_; # Closure zurück
              *rest_arg_ = rest_arg;
            }}
            else
            # restliche Argumente vergessen:
            { set_args_end_pointer(rest_args_pointer); }
        }
        else
        # Closure ohne &REST-Flag: restliche Argumente vergessen:
        { set_args_end_pointer(rest_args_pointer); }
      return closure;
    }}


#           ----------------------- E V A L -----------------------

# später:
  local Values eval1 (object form);
  local Values eval_fsubr (object fun, object args);
  local Values eval_applyhook (object fun);
  local Values eval_subr (object fun);
  local Values eval_closure (object fun);
  #ifdef DYNAMIC_FFI
  local Values eval_ffunction (object fun);
  #endif

# UP: Wertet eine Form im aktuellen Environment aus.
# eval(form);
# > form: Form
# < mv_count/mv_space: Werte
# can trigger GC
  global Values eval (object form);
  global Values eval(form)
    var object form;
    { start:
      # Test auf Tastatur-Interrupt:
      interruptp(
        { pushSTACK(form); # form retten
          pushSTACK(S(eval)); tast_break(); # Break-Schleife aufrufen
          form = popSTACK();
          goto start;
        });
     {var sp_jmp_buf my_jmp_buf;
      # EVAL-Frame aufbauen:
      { var object* top_of_frame = STACK; # Pointer übern Frame
        pushSTACK(form); # Form
        finish_entry_frame(EVAL,&!my_jmp_buf,,
          { if (mv_count==0) # nach Wiedereintritt: Form übergeben?
              { form = STACK_(frame_form); } # selbe Form nochmal evaluieren
              else
              { form = STACK_(frame_form) = value1; } # übergebene Form evaluieren
          });
      }
      # Test auf *EVALHOOK*:
      { var object evalhook_value = Symbol_value(S(evalhookstern)); # *EVALHOOK*
        if (nullp(evalhook_value)) # *EVALHOOK* = NIL ?
          # ja -> normal weiter-evaluieren
          { pushSTACK(Symbol_value(S(applyhookstern))); eval1(form); }
          else
          { # *EVALHOOK*, *APPLYHOOK* an NIL binden:
            bindhooks_NIL();
            # (FUNCALL *EVALHOOK* form env) ausführen:
            pushSTACK(form); # Form als 1. Argument
            pushSTACK(evalhook_value); # Funktion retten
           {var environment* stack_env = nest_aktenv(); # Environments in den Stack,
            var object env = allocate_vector(5); # in neu allozierten Vektor
            *(environment*)(&TheSvector(env)->data[0]) = *stack_env; # hineinschieben
            skipSTACK(5);
            evalhook_value = popSTACK(); # Funktion zurück
            pushSTACK(env); # gesamtes Environment als 2. Argument
            funcall(evalhook_value,2);
            # alte Werte von *EVALHOOK*, *APPLYHOOK* zurück:
            unwind();
            # EVAL-Frame auflösen:
            unwind();
      }   }}
    }}

# UP: Wertet eine Form im aktuellen Environment aus. Nimmt dabei auf
# *EVALHOOK* und *APPLYHOOK* keine Rücksicht.
# eval_no_hooks(form);
# > form: Form
# < mv_count/mv_space: Werte
# can trigger GC
  global Values eval_no_hooks (object form);
  global Values eval_no_hooks(form)
    var object form;
    { var sp_jmp_buf my_jmp_buf;
      # EVAL-Frame aufbauen:
      { var object* top_of_frame = STACK; # Pointer übern Frame
        pushSTACK(form); # Form
        finish_entry_frame(EVAL,&!my_jmp_buf,,
          { if (mv_count==0) # nach Wiedereintritt: Form übergeben?
              { form = STACK_(frame_form); } # selbe Form nochmal evaluieren
              else
              { form = STACK_(frame_form) = value1; } # übergebene Form evaluieren
          });
      }
      # weiterevaluieren, *APPLYHOOK* als NIL betrachten:
      { pushSTACK(NIL); eval1(form); }
    }

# UP: Stellt fest, ob eine non-Standard Form self-evaluating ist.
  local boolean other_self_evaluating_p (void);
  local boolean other_self_evaluating_p()
    { # (member (find-package "COMMON-LISP") (package-use-list *package*))
      var object pack = find_package(O(common_lisp_string));
      if (!nullp(pack))
        { var object list = ThePackage(get_current_package())->pack_use_list;
          while (consp(list))
            { if (eq(Car(list),pack)) { return TRUE; }
              list = Cdr(list);
        }   }
      return FALSE;
    }

# UP: Wertet eine Form im aktuellen Environment aus.
# Nimmt dabei auf *EVALHOOK* keine Rücksicht, und erwartet den Wert von
# *APPLYHOOK*.
# Der EVAL-Frame muss bereits aufgebaut sein; er wird dann abgebaut.
# eval1(form);
# > form: Form
# > STACK_3..STACK_1: EVAL-Frame, mit Form in STACK_3
# > STACK_0: Wert von *APPLYHOOK*
# < mv_count/mv_space: Werte
# verändert STACK
# can trigger GC
  local Values eval1(form)
    var object form;
    { if (atomp(form))
        { if (symbolp(form))
            { # Form ist Symbol
              value1 = sym_value(form,aktenv.var_env); # Wert im aktuellen Environment
              if (eq(value1,unbound))
                { pushSTACK(form); # Wert für Slot NAME von CELL-ERROR
                  pushSTACK(form);
                  fehler(unbound_variable,
                         GETTEXT("EVAL: variable ~ has no value")
                        );
                }
              elif (symbolmacrop(value1)) # Symbol-Macro?
                # ja -> expandieren und erneut evaluieren:
                { skipSTACK(1); # Wert von *APPLYHOOK* vergessen
                  check_SP(); check_STACK();
                  eval(TheSymbolmacro(value1)->symbolmacro_expansion); # Expansion evaluieren
                  unwind(); # EVAL-Frame auflösen
                }
              else
                { mv_count=1; # value1 als Wert
                  skipSTACK(1);
                  unwind(); # EVAL-Frame auflösen
                }
            }
          elif (   numberp(form) # Zahl ?
                || charp(form) # Character ?
                || arrayp(form) # Array ?
                || other_self_evaluating_p() # X3J13 vote <72> erwünscht?
               )
            # self-evaluating form
            { value1 = form; mv_count=1; # form als Wert
              skipSTACK(1);
              unwind(); # EVAL-Frame auflösen
            }
          else
            { pushSTACK(form);
              fehler(source_program_error,
                     GETTEXT("EVAL: illegal form ~")
                    );
            }
        }
        else
        # Form ist ein Cons
        { # Feststellen, ob Macro-call, evtl. expandieren:
          macroexp(form,aktenv.var_env,aktenv.fun_env); form = value1;
          if (!nullp(value2)) # expandiert ?
            # jetzt erst richtig evaluieren:
            { skipSTACK(1); # Wert von *APPLYHOOK* vergessen
              check_SP(); check_STACK();
              eval(form); # expandierte Form evaluieren
              unwind(); # EVAL-Frame auflösen
            }
            else
            { var object fun = Car(form); # Funktionsbezeichnung
              if (funnamep(fun))
                { # Funktionsdefinition im Environment holen:
                  fun = sym_function(fun,aktenv.fun_env);
                  # je nach Typ der Funktion verzweigen:
                  # unbound / SUBR/FSUBR/Closure / Macro-Cons
                  #ifdef TYPECODES
                  switch (typecode(fun))
                  #else
                  if (subrp(fun)) { goto case_subr; }
                  elif (orecordp(fun)) { goto case_orecord; }
                  else switch (0)
                  #endif
                    { case_subr: # SUBR
                        pushSTACK(Cdr(form)); # Argumentliste
                        if (!nullp(STACK_1)) goto applyhook;
                        eval_subr(fun);
                        break;
                      case_closure: # Closure
                        pushSTACK(Cdr(form)); # Argumentliste
                        closure: # fun ist eine Closure
                        if (!nullp(STACK_1)) goto applyhook;
                        eval_closure(fun);
                        break;
                      applyhook: # Wert von *APPLYHOOK* ist /= NIL.
                        eval_applyhook(fun);
                        break;
                      case_orecord:
                        switch (Record_type(fun))
                          { case_Rectype_Closure_above;
                            case Rectype_Fsubr:
                              # Fsubr
                              eval_fsubr(fun,Cdr(form));
                              break;
                            #ifdef DYNAMIC_FFI
                            case Rectype_Ffunction:
                              # Foreign-Function
                              pushSTACK(Cdr(form)); # Argumentliste
                              if (!nullp(STACK_1)) goto applyhook;
                              eval_ffunction(fun);
                              break;
                            #endif
                            default:
                              goto undef;
                          }
                        break;
                      default: undef:
                        fehler_undefined(S(eval),Car(form));
                }   }
              elif (consp(fun) && eq(Car(fun),S(lambda))) # Lambda-Ausdruck?
                { pushSTACK(Cdr(form)); # Argumentliste
                  fun = get_closure(Cdr(fun),S(Klambda),FALSE,&aktenv); # Closure im aktuellen Environment erzeugen
                  goto closure; # und diese auf die Argumente anwenden, wie oben
                }
              else
                { pushSTACK(fun);
                  pushSTACK(S(eval));
                  fehler(source_program_error,
                         GETTEXT("~: ~ is not a function name")
                        );
                }
            }
        }
    }

# In EVAL: Wendet ein FSUBR auf eine Argumentliste an, räumt den STACK auf
# und liefert die Werte.
# eval_fsubr(fun,args);
# > fun: ein FSUBR
# > args: Argumentliste
# > STACK-Aufbau: EVAL-Frame, *APPLYHOOK*.
# < STACK: aufgeräumt
# < mv_count/mv_space: Werte
# verändert STACK
# can trigger GC
  local Values eval_fsubr(fun,args)
    var object fun;
    var object args;
    { skipSTACK(1); # Wert von *APPLYHOOK* vergessen
      check_SP(); check_STACK();
     #if STACKCHECKS
     {var object* STACKbefore = STACK;
     #endif
      # Argumente in den STACK legen:
      switch ((uintW)posfixnum_to_L(TheFsubr(fun)->argtype))
        { # Macro für 1 required-Parameter:
          #define REQ_PAR()  \
            { if (atomp(args)) goto fehler_zuwenig;                   \
              pushSTACK(Car(args)); # nächster Parameter in den STACK \
              args = Cdr(args);                                       \
            }
          case (uintW)fsubr_argtype_2_0_nobody:
            # FSUBR mit 2 required-Parametern
            REQ_PAR();
          case (uintW)fsubr_argtype_1_0_nobody:
            # FSUBR mit 1 required-Parameter
            REQ_PAR();
            if (!nullp(args)) goto fehler_zuviel;
            break;
          case (uintW)fsubr_argtype_2_1_nobody:
            # FSUBR mit 2 required-Parametern und 1 optional-Parameter
            REQ_PAR();
          case (uintW)fsubr_argtype_1_1_nobody:
            # FSUBR mit 1 required-Parameter und 1 optional-Parameter
            REQ_PAR();
            if (consp(args))
              { pushSTACK(Car(args)); # optionalen Parameter in den STACK
                args = Cdr(args);
                if (!nullp(args)) goto fehler_zuviel;
              }
              else
              { pushSTACK(unbound); # unbound stattdessen in den STACK
                if (!nullp(args)) goto fehler_dotted;
              }
            break;
          case (uintW)fsubr_argtype_2_body:
            # FSUBR mit 2 required-Parametern und Body-Parameter
            REQ_PAR();
          case (uintW)fsubr_argtype_1_body:
            # FSUBR mit 1 required-Parameter und Body-Parameter
            REQ_PAR();
          case (uintW)fsubr_argtype_0_body:
            # FSUBR mit 0 required-Parametern und Body-Parameter
            pushSTACK(args); # restlichen Body in den STACK
            break;
          default: NOTREACHED
          fehler_zuwenig: # Argumentliste args ist vorzeitig ein Atom
            if (!nullp(args)) goto fehler_dotted;
            # STACK bis zum aufrufenden EVAL-Frame aufräumen:
            until (framecode(STACK_0) & bit(frame_bit_t)) { skipSTACK(1); }
            { var object form = STACK_(frame_form); # Form aus dem EVAL-Frame
              pushSTACK(form);
              pushSTACK(Car(form));
              fehler(source_program_error,
                     GETTEXT("EVAL: too few parameters for special-form ~: ~")
                    );
            }
          fehler_zuviel: # Argumentliste args ist am Schluss nicht NIL
            if (atomp(args)) goto fehler_dotted;
            # STACK bis zum aufrufenden EVAL-Frame aufräumen:
            until (framecode(STACK_0) & bit(frame_bit_t)) { skipSTACK(1); }
            { var object form = STACK_(frame_form); # Form aus dem EVAL-Frame
              pushSTACK(form);
              pushSTACK(Car(form));
              fehler(source_program_error,
                     GETTEXT("EVAL: too many parameters for special-form ~: ~")
                    );
            }
          fehler_dotted: # Argumentliste args endet mit Atom /= NIL
            # STACK bis zum aufrufenden EVAL-Frame aufräumen:
            until (framecode(STACK_0) & bit(frame_bit_t)) { skipSTACK(1); }
            { var object form = STACK_(frame_form); # Form aus dem EVAL-Frame
              pushSTACK(form);
              pushSTACK(Car(form));
              fehler(source_program_error,
                     GETTEXT("EVAL: dotted parameter list for special form ~: ~")
                    );
            }
          #undef REQ_PAR
        }
      # FSUBR selbst aufrufen:
      subr_self = fun;
      (*(fsubr_function*)(TheFsubr(fun)->function))();
     #if STACKCHECKS
      if (!(STACK == STACKbefore)) # STACK so wie vorher?
        { abort(); } # nein -> ab in den Debugger
     }
     #endif
      unwind(); # EVAL-Frame auflösen
    }

# In EVAL: Wendet *APPLYHOOK* auf eine Funktion (SUBR oder Closure) und
# eine Argumentliste an, räumt den STACK auf und liefert die Werte.
# eval_applyhook(fun);
# > fun: Funktion, ein SUBR oder eine Closure
# > STACK-Aufbau: EVAL-Frame, *APPLYHOOK* (/= NIL), Argumentliste.
# < STACK: aufgeräumt
# < mv_count/mv_space: Werte
# verändert STACK
# can trigger GC
  local Values eval_applyhook(fun)
    var object fun;
    { var object args = popSTACK(); # Argumentliste
      var object applyhook_value = popSTACK(); # Wert von *APPLYHOOK*
      check_SP();
      # *EVALHOOK*, *APPLYHOOK* an NIL binden:
      bindhooks_NIL();
      #ifndef X3J13_005
      # (FUNCALL *APPLYHOOK* fun args env) ausführen:
      pushSTACK(fun); # Funktion als 1. Argument
      pushSTACK(args); # Argumentliste als 2. Argument
      pushSTACK(applyhook_value); # Funktion retten
      {var environment* stack_env = nest_aktenv(); # Environments in den Stack,
       var object env = allocate_vector(5); # in neu allozierten Vektor
       *(environment*)(&TheSvector(env)->data[0]) = *stack_env; # hineinschieben
       skipSTACK(5);
      }
      applyhook_value = popSTACK(); # Funktion zurück
      pushSTACK(env); # gesamtes Environment als 3. Argument
      funcall(applyhook_value,3);
      #else
      # (FUNCALL *APPLYHOOK* fun args) ausführen:
      pushSTACK(fun); # Funktion als 1. Argument
      pushSTACK(args); # Argumentliste als 2. Argument
      funcall(applyhook_value,2);
      #endif
      # alte Werte von *EVALHOOK*, *APPLYHOOK* zurück:
      unwind();
      # EVAL-Frame auflösen:
      unwind();
    }

# In EVAL: Fehler bei zu wenig Argumenten
  nonreturning_function(local, fehler_eval_zuwenig, (object fun));
  local void fehler_eval_zuwenig(fun)
    var object fun;
    { var object form = STACK_(frame_form); # Form
      pushSTACK(form);
      pushSTACK(fun);
      fehler(source_program_error,
             GETTEXT("EVAL: too few arguments given to ~: ~")
            );
    }

# In EVAL: Fehler bei zu vielen Argumenten
  nonreturning_function(local, fehler_eval_zuviel, (object fun));
  local void fehler_eval_zuviel(fun)
    var object fun;
    { var object form = STACK_(frame_form); # Form
      pushSTACK(form);
      pushSTACK(fun);
      fehler(source_program_error,
             GETTEXT("EVAL: too many arguments given to ~: ~")
            );
    }

# In EVAL: Fehler bei punktierter Argumentliste
  nonreturning_function(local, fehler_eval_dotted, (object fun));
  local void fehler_eval_dotted(fun)
    var object fun;
    { var object form = STACK_(frame_form); # Form
      pushSTACK(form);
      pushSTACK(fun);
      fehler(source_program_error,
             GETTEXT("EVAL: argument list given to ~ is dotted: ~")
            );
    }

# In EVAL: Wendet ein SUBR auf eine Argumentliste an, räumt den STACK auf
# und liefert die Werte.
# eval_subr(fun);
# > fun: Funktion, ein SUBR
# > STACK-Aufbau: EVAL-Frame, *APPLYHOOK*, Argumentliste.
# < STACK: aufgeräumt
# < mv_count/mv_space: Werte
# verändert STACK
# can trigger GC
  local Values eval_subr(fun)
    var object fun;
    { var object args = popSTACK(); # Argumentliste
      skipSTACK(1); # Wert von *APPLYHOOK* vergessen
      check_SP(); check_STACK();
     {var object* args_pointer = args_end_pointer; # Pointer über die Argumente
      var object* rest_args_pointer; # Pointer über die restlichen Argumente
      var uintL argcount; # Anzahl der restlichen Argumente
      # Argumente ausgewertet in den STACK legen:
      # erst ein Dispatch für die wichtigsten Fälle:
      switch (TheSubr(fun)->argtype)
        { # Macro für ein required-Argument:
          #define REQ_ARG()  \
            { if (atomp(args)) goto fehler_zuwenig;                \
              pushSTACK(Cdr(args)); # restliche Argumente          \
              eval(Car(args)); # nächstes Argument auswerten       \
              args = STACK_0; STACK_0 = value1; # und in den STACK \
            }
          # Macro für das n-letzte optional-Argument:
          #define OPT_ARG(n)  \
            { if (atomp(args)) goto unbound_optional_##n ;         \
              pushSTACK(Cdr(args)); # restliche Argumente          \
              eval(Car(args)); # nächstes Argument auswerten       \
              args = STACK_0; STACK_0 = value1; # und in den STACK \
            }
          case (uintW)subr_argtype_6_0:
            # SUBR mit 6 required-Argumenten
            REQ_ARG();
          case (uintW)subr_argtype_5_0:
            # SUBR mit 5 required-Argumenten
            REQ_ARG();
          case (uintW)subr_argtype_4_0:
            # SUBR mit 4 required-Argumenten
            REQ_ARG();
          case (uintW)subr_argtype_3_0:
            # SUBR mit 3 required-Argumenten
            REQ_ARG();
          case (uintW)subr_argtype_2_0:
            # SUBR mit 2 required-Argumenten
            REQ_ARG();
          case (uintW)subr_argtype_1_0:
            # SUBR mit 1 required-Argument
            REQ_ARG();
          case (uintW)subr_argtype_0_0:
            # SUBR ohne Argumente
            if (!nullp(args)) goto fehler_zuviel;
            goto apply_subr_norest;
          case (uintW)subr_argtype_4_1:
            # SUBR mit 4 required-Argumenten und 1 optional-Argument
            REQ_ARG();
          case (uintW)subr_argtype_3_1:
            # SUBR mit 3 required-Argumenten und 1 optional-Argument
            REQ_ARG();
          case (uintW)subr_argtype_2_1:
            # SUBR mit 2 required-Argumenten und 1 optional-Argument
            REQ_ARG();
          case (uintW)subr_argtype_1_1:
            # SUBR mit 1 required-Argument und 1 optional-Argument
            REQ_ARG();
          case (uintW)subr_argtype_0_1:
            # SUBR mit 1 optional-Argument
            OPT_ARG(1);
            if (!nullp(args)) goto fehler_zuviel;
            goto apply_subr_norest;
          case (uintW)subr_argtype_2_2:
            # SUBR mit 2 required-Argumenten und 2 optional-Argumenten
            REQ_ARG();
          case (uintW)subr_argtype_1_2:
            # SUBR mit 1 required-Argument und 2 optional-Argumenten
            REQ_ARG();
          case (uintW)subr_argtype_0_2:
            # SUBR mit 2 optional-Argumenten
            OPT_ARG(2);
            OPT_ARG(1);
            if (!nullp(args)) goto fehler_zuviel;
            goto apply_subr_norest;
          case (uintW)subr_argtype_2_3:
            # SUBR mit 2 required-Argumenten und 3 optional-Argumenten
            REQ_ARG();
          case (uintW)subr_argtype_1_3:
            # SUBR mit 1 required-Argument und 3 optional-Argumenten
            REQ_ARG();
          case (uintW)subr_argtype_0_3:
            # SUBR mit 3 optional-Argumenten
            OPT_ARG(3);
            OPT_ARG(2);
            OPT_ARG(1);
            if (!nullp(args)) goto fehler_zuviel;
            goto apply_subr_norest;
          case (uintW)subr_argtype_0_5:
            # SUBR mit 5 optional-Argumenten
            OPT_ARG(5);
          case (uintW)subr_argtype_0_4:
            # SUBR mit 4 optional-Argumenten
            OPT_ARG(4);
            OPT_ARG(3);
            OPT_ARG(2);
            OPT_ARG(1);
            if (!nullp(args)) goto fehler_zuviel;
            goto apply_subr_norest;
          unbound_optional_5: # Noch 5 optionale Argumente, aber atomp(args)
            pushSTACK(unbound);
          unbound_optional_4: # Noch 4 optionale Argumente, aber atomp(args)
            pushSTACK(unbound);
          unbound_optional_3: # Noch 3 optionale Argumente, aber atomp(args)
            pushSTACK(unbound);
          unbound_optional_2: # Noch 2 optionale Argumente, aber atomp(args)
            pushSTACK(unbound);
          unbound_optional_1: # Noch 1 optionales Argument, aber atomp(args)
            pushSTACK(unbound);
            if (!nullp(args)) goto fehler_dotted;
            goto apply_subr_norest;
          case (uintW)subr_argtype_3_0_rest:
            # SUBR mit 3 required-Argumenten und weiteren Argumenten
            REQ_ARG();
          case (uintW)subr_argtype_2_0_rest:
            # SUBR mit 2 required-Argumenten und weiteren Argumenten
            REQ_ARG();
          case (uintW)subr_argtype_1_0_rest:
            # SUBR mit 1 required-Argument und weiteren Argumenten
            REQ_ARG();
          case (uintW)subr_argtype_0_0_rest:
            # SUBR mit weiteren Argumenten
            rest_args_pointer = args_end_pointer; # Pointer über die restlichen Argumente
            # alle weiteren Argumente auswerten und in den Stack:
            argcount = 0; # Zähler für die restlichen Argumente
            while (consp(args))
              { check_STACK();
                pushSTACK(Cdr(args)); # restliche Argumente
                eval(Car(args)); # nächstes Argument auswerten
                args = STACK_0; STACK_0 = value1; # und in den STACK
                argcount++;
              }
            goto apply_subr_rest;
          case (uintW)subr_argtype_4_0_key:
            # SUBR mit 4 required-Argumenten und Keyword-Argumenten
            REQ_ARG();
          case (uintW)subr_argtype_3_0_key:
            # SUBR mit 3 required-Argumenten und Keyword-Argumenten
            REQ_ARG();
          case (uintW)subr_argtype_2_0_key:
            # SUBR mit 2 required-Argumenten und Keyword-Argumenten
            REQ_ARG();
          case (uintW)subr_argtype_1_0_key:
            # SUBR mit 1 required-Argument und Keyword-Argumenten
            REQ_ARG();
          case (uintW)subr_argtype_0_0_key:
            # SUBR mit Keyword-Argumenten
            if (atomp(args)) goto unbound_optional_key_0;
            goto apply_subr_key;
          case (uintW)subr_argtype_1_1_key:
            # SUBR mit 1 required-Argument, 1 optional-Argument und Keyword-Argumenten
            REQ_ARG();
          case (uintW)subr_argtype_0_1_key:
            # SUBR mit 1 optional-Argument und Keyword-Argumenten
            OPT_ARG(key_1);
            if (atomp(args)) goto unbound_optional_key_0;
            goto apply_subr_key;
          case (uintW)subr_argtype_1_2_key:
            # SUBR mit 1 required-Argument, 2 optional-Argumenten und Keyword-Argumenten
            REQ_ARG();
            OPT_ARG(key_2);
            OPT_ARG(key_1);
            if (atomp(args)) goto unbound_optional_key_0;
            goto apply_subr_key;
          unbound_optional_key_2: # Noch 2 optionale Argumente, aber atomp(args)
            pushSTACK(unbound);
          unbound_optional_key_1: # Noch 1 optionales Argument, aber atomp(args)
            pushSTACK(unbound);
          unbound_optional_key_0: # Vor den Keywords ist atomp(args)
            { var uintC count;
              dotimesC(count,TheSubr(fun)->key_anz, { pushSTACK(unbound); } );
            }
            if (!nullp(args)) goto fehler_dotted;
            goto apply_subr_norest;
          default: NOTREACHED
          #undef OPT_ARG
          #undef REQ_ARG
        }
      # Nun die allgemeine Version:
      # Platz auf dem STACK reservieren:
      get_space_on_STACK(sizeof(object) *
                         (uintL)(TheSubr(fun)->req_anz +
                                 TheSubr(fun)->opt_anz +
                                 TheSubr(fun)->key_anz));
      # required Parameter auswerten und in den Stack ablegen:
      { var uintC count;
        dotimesC(count,TheSubr(fun)->req_anz,
          { if (atomp(args)) goto fehler_zuwenig; # Argumentliste zu Ende?
            pushSTACK(Cdr(args)); # restliche Argumentliste
            eval(Car(args)); # nächstes Argument auswerten
            args = STACK_0; STACK_0 = value1; # und in den Stack
          });
      }
      # optionale Parameter auswerten und in den Stack ablegen:
      { var uintC count = TheSubr(fun)->opt_anz;
        loop
          { if (atomp(args)) break; # Argumentliste zu Ende?
            if (count==0) goto optionals_ok; # alle optionalen Parameter versorgt?
            count--;
            pushSTACK(Cdr(args)); # restliche Argumentliste
            eval(Car(args)); # nächstes Argument auswerten
            args = STACK_0; STACK_0 = value1; # und in den Stack
          }
        # Argumentliste beendet.
        # Alle weiteren count optionalen Parameter bekommen den "Wert"
        # #<UNBOUND>, auch die Keyword-Parameter:
        dotimesC(count,count + TheSubr(fun)->key_anz, { pushSTACK(unbound); } );
        if (TheSubr(fun)->rest_flag == subr_rest) # &REST-Flag?
          # ja -> 0 zusätzliche Argumente:
          { argcount = 0; rest_args_pointer = args_end_pointer; }
          # nein -> nichts zu tun
        goto los;
      }
      optionals_ok:
      # Rest- und Keyword-Parameter behandeln.
      # args = restliche Argumentliste (noch nicht zu Ende)
      if (TheSubr(fun)->key_flag == subr_nokey)
        # SUBR ohne KEY
        { if (TheSubr(fun)->rest_flag == subr_norest)
            # SUBR ohne REST oder KEY -> Argumentliste müsste zu Ende sein
            { goto fehler_zuviel; }
            else
            # SUBR mit nur REST, ohne KEY: Behandlung der restlichen Argumente
            { rest_args_pointer = args_end_pointer;
              argcount = 0; # Zähler für die restlichen Argumente
              do { check_STACK();
                   pushSTACK(Cdr(args)); # restliche Argumentliste
                   eval(Car(args)); # nächstes Argument auswerten
                   args = STACK_0; STACK_0 = value1; # und in den Stack
                   argcount++;
                 }
                 while (consp(args));
              if (((uintL)~(uintL)0 > ca_limit_1) && (argcount > ca_limit_1))
                { goto fehler_zuviel; }
        }   }
        else
        # SUBR mit Keywords.
        apply_subr_key:
        # args = restliche Argumentliste (noch nicht zu Ende)
        # Erst die Keyword-Parameter mit #<UNBOUND> vorbesetzen, dann
        # die restlichen Argumente auswerten und im Stack ablegen, dann
        # die Keywords zuordnen:
        { var object* key_args_pointer = args_end_pointer; # Pointer über Keyword-Parameter
          # alle Keyword-Parameter mit #<UNBOUND> vorbesetzen:
          { var uintC count;
            dotimesC(count,TheSubr(fun)->key_anz, { pushSTACK(unbound); } );
          }
          rest_args_pointer = args_end_pointer; # Pointer über die restlichen Argumente
          # alle weiteren Argumente auswerten und in den Stack:
          argcount = 0; # Zähler für die restlichen Argumente
          do { check_STACK();
               pushSTACK(Cdr(args)); # restliche Argumentliste
               eval(Car(args)); # nächstes Argument auswerten
               args = STACK_0; STACK_0 = value1; # und in den Stack
               argcount++;
             }
             while (consp(args));
          if (((uintL)~(uintL)0 > ca_limit_1) && (argcount > ca_limit_1))
            { goto fehler_zuviel; }
          # Keywords zuordnen und evtl. restliche Argumente wegwerfen:
          match_subr_key(fun,argcount,key_args_pointer,rest_args_pointer);
        }
      los: # Funktion anspringen
      # restliche Argumentliste muss =NIL sein:
      if (!nullp(args)) goto fehler_dotted;
      if (TheSubr(fun)->rest_flag == subr_norest)
        # SUBR ohne &REST-Flag:
        apply_subr_norest:
        { subr_self = fun;
          (*(subr_norest_function*)(TheSubr(fun)->function))();
        }
        else
        # SUBR mit &REST-Flag:
        apply_subr_rest:
        { subr_self = fun;
          (*(subr_rest_function*)(TheSubr(fun)->function))
           (argcount,rest_args_pointer);
        }
      #if STACKCHECKS
      if (!(args_pointer == args_end_pointer)) # Stack aufgeräumt?
        { abort(); } # nein -> ab in den Debugger
      #endif
      unwind(); # EVAL-Frame auflösen
      return; # fertig
      # Gesammelte Fehlermeldungen:
      fehler_zuwenig: # Argumentliste args ist vorzeitig ein Atom
        if (!nullp(args)) goto fehler_dotted;
        set_args_end_pointer(args_pointer); # STACK aufräumen
        fehler_eval_zuwenig(TheSubr(fun)->name);
      fehler_zuviel: # Argumentliste args ist am Schluss nicht NIL
        if (atomp(args)) goto fehler_dotted;
        set_args_end_pointer(args_pointer); # STACK aufräumen
        fehler_eval_zuviel(TheSubr(fun)->name);
      fehler_dotted: # Argumentliste args endet mit Atom /= NIL
        set_args_end_pointer(args_pointer); # STACK aufräumen
        fehler_eval_dotted(TheSubr(fun)->name);
    }}

# In EVAL: Wendet eine Closure auf eine Argumentliste an, räumt den STACK auf
# und liefert die Werte.
# eval_closure(fun);
# > fun: Funktion, eine Closure
# > STACK-Aufbau: EVAL-Frame, *APPLYHOOK*, Argumentliste.
# < STACK: aufgeräumt
# < mv_count/mv_space: Werte
# verändert STACK
# can trigger GC
  local Values eval_closure(closure)
    var object closure;
    { var object args = popSTACK(); # Argumentliste
      skipSTACK(1); # Wert von *APPLYHOOK* vergessen
      # STACK-Aufbau: EVAL-Frame.
      check_SP(); check_STACK();
      pushSTACK(closure); # Closure retten
     {var object* closure_ = &STACK_0; # und merken, wo sie sitzt
      if (simple_bit_vector_p(TheClosure(closure)->clos_codevec))
        # closure ist eine compilierte Closure
        { var object* STACKbefore = STACK;
          var object codevec = TheCclosure(closure)->clos_codevec; # Code-Vektor
          # Argumente ausgewertet in den STACK legen:
          # erst ein Dispatch für die wichtigsten Fälle:
          switch (TheCodevec(codevec)->ccv_signature)
            { # Macro für ein required-Argument:
              #define REQ_ARG()  \
                { if (atomp(args)) goto fehler_zuwenig;                \
                  pushSTACK(Cdr(args)); # restliche Argumente          \
                  eval(Car(args)); # nächstes Argument auswerten       \
                  args = STACK_0; STACK_0 = value1; # und in den STACK \
                }
              # Macro für das n-letzte optional-Argument:
              #define OPT_ARG(n)  \
                { if (atomp(args)) goto unbound_optional_##n ;         \
                  pushSTACK(Cdr(args)); # restliche Argumente          \
                  eval(Car(args)); # nächstes Argument auswerten       \
                  args = STACK_0; STACK_0 = value1; # und in den STACK \
                }
              case (uintB)cclos_argtype_5_0:
                # 5 required-Argumente
                REQ_ARG();
              case (uintB)cclos_argtype_4_0:
                # 4 required-Argumente
                REQ_ARG();
              case (uintB)cclos_argtype_3_0:
                # 3 required-Argumente
                REQ_ARG();
              case (uintB)cclos_argtype_2_0:
                # 2 required-Argumente
                REQ_ARG();
              case (uintB)cclos_argtype_1_0:
                # 1 required-Argument
                REQ_ARG();
              case (uintB)cclos_argtype_0_0:
                # keine Argumente
                noch_0_opt_args:
                if (!nullp(args)) goto fehler_zuviel;
                goto apply_cclosure_nokey;
              case (uintB)cclos_argtype_4_1:
                # 4 required-Argumente und 1 optional-Argument
                REQ_ARG();
              case (uintB)cclos_argtype_3_1:
                # 3 required-Argumente und 1 optional-Argument
                REQ_ARG();
              case (uintB)cclos_argtype_2_1:
                # 2 required-Argumente und 1 optional-Argument
                REQ_ARG();
              case (uintB)cclos_argtype_1_1:
                # 1 required-Argument und 1 optional-Argument
                REQ_ARG();
              case (uintB)cclos_argtype_0_1:
                # 1 optional-Argument
                noch_1_opt_args:
                OPT_ARG(1);
                goto noch_0_opt_args;
              case (uintB)cclos_argtype_3_2:
                # 3 required-Argumente und 2 optional-Argumente
                REQ_ARG();
              case (uintB)cclos_argtype_2_2:
                # 2 required-Argumente und 2 optional-Argumente
                REQ_ARG();
              case (uintB)cclos_argtype_1_2:
                # 1 required-Argument und 2 optional-Argumente
                REQ_ARG();
              case (uintB)cclos_argtype_0_2:
                # 2 optional-Argumente
                noch_2_opt_args:
                OPT_ARG(2);
                goto noch_1_opt_args;
              case (uintB)cclos_argtype_2_3:
                # 2 required-Argumente und 3 optional-Argumente
                REQ_ARG();
              case (uintB)cclos_argtype_1_3:
                # 1 required-Argument und 3 optional-Argumente
                REQ_ARG();
              case (uintB)cclos_argtype_0_3:
                # 3 optional-Argumente
                noch_3_opt_args:
                OPT_ARG(3);
                goto noch_2_opt_args;
              case (uintB)cclos_argtype_1_4:
                # 1 required-Argument und 4 optional-Argumente
                REQ_ARG();
              case (uintB)cclos_argtype_0_4:
                # 4 optional-Argumente
                noch_4_opt_args:
                OPT_ARG(4);
                goto noch_3_opt_args;
              case (uintB)cclos_argtype_0_5:
                # 5 optional-Argumente
                OPT_ARG(5);
                goto noch_4_opt_args;
              unbound_optional_5: # Noch 5 optionale Argumente, aber atomp(args)
                pushSTACK(unbound);
              unbound_optional_4: # Noch 4 optionale Argumente, aber atomp(args)
                pushSTACK(unbound);
              unbound_optional_3: # Noch 3 optionale Argumente, aber atomp(args)
                pushSTACK(unbound);
              unbound_optional_2: # Noch 2 optionale Argumente, aber atomp(args)
                pushSTACK(unbound);
              unbound_optional_1: # Noch 1 optionales Argument, aber atomp(args)
                pushSTACK(unbound);
                if (!nullp(args)) goto fehler_dotted;
                goto apply_cclosure_nokey;
              case (uintB)cclos_argtype_4_0_rest:
                # 4 required-Argumente, Rest-Parameter
                REQ_ARG();
              case (uintB)cclos_argtype_3_0_rest:
                # 3 required-Argumente, Rest-Parameter
                REQ_ARG();
              case (uintB)cclos_argtype_2_0_rest:
                # 2 required-Argumente, Rest-Parameter
                REQ_ARG();
              case (uintB)cclos_argtype_1_0_rest:
                # 1 required-Argument, Rest-Parameter
                REQ_ARG();
              case (uintB)cclos_argtype_0_0_rest:
                # keine Argumente, Rest-Parameter
                if (consp(args)) goto apply_cclosure_rest_nokey;
                if (!nullp(args)) goto fehler_dotted;
                pushSTACK(NIL); # Rest-Parameter := NIL
                goto apply_cclosure_nokey;
              case (uintB)cclos_argtype_4_0_key:
                # 4 required-Argumente, Keyword-Argumente
                REQ_ARG();
              case (uintB)cclos_argtype_3_0_key:
                # 3 required-Argumente, Keyword-Argumente
                REQ_ARG();
              case (uintB)cclos_argtype_2_0_key:
                # 2 required-Argumente, Keyword-Argumente
                REQ_ARG();
              case (uintB)cclos_argtype_1_0_key:
                # 1 required-Argument, Keyword-Argumente
                REQ_ARG();
                noch_0_opt_args_key:
                closure = *closure_; codevec = TheCclosure(closure)->clos_codevec;
              case (uintB)cclos_argtype_0_0_key:
                # nur Keyword-Argumente
                if (atomp(args)) goto unbound_optional_key_0;
                goto apply_cclosure_key;
              case (uintB)cclos_argtype_3_1_key:
                # 3 required-Argumente und 1 optional-Argument, Keyword-Argumente
                REQ_ARG();
              case (uintB)cclos_argtype_2_1_key:
                # 2 required-Argumente und 1 optional-Argument, Keyword-Argumente
                REQ_ARG();
              case (uintB)cclos_argtype_1_1_key:
                # 1 required-Argument und 1 optional-Argument, Keyword-Argumente
                REQ_ARG();
              case (uintB)cclos_argtype_0_1_key:
                # 1 optional-Argument, Keyword-Argumente
                noch_1_opt_args_key:
                OPT_ARG(key_1);
                goto noch_0_opt_args_key;
              case (uintB)cclos_argtype_2_2_key:
                # 2 required-Argumente und 2 optional-Argumente, Keyword-Argumente
                REQ_ARG();
              case (uintB)cclos_argtype_1_2_key:
                # 1 required-Argument und 2 optional-Argumente, Keyword-Argumente
                REQ_ARG();
              case (uintB)cclos_argtype_0_2_key:
                # 2 optional-Argumente, Keyword-Argumente
                noch_2_opt_args_key:
                OPT_ARG(key_2);
                goto noch_1_opt_args_key;
              case (uintB)cclos_argtype_1_3_key:
                # 1 required-Argument und 3 optional-Argumente, Keyword-Argumente
                REQ_ARG();
              case (uintB)cclos_argtype_0_3_key:
                # 3 optional-Argumente, Keyword-Argumente
                noch_3_opt_args_key:
                OPT_ARG(key_3);
                goto noch_2_opt_args_key;
              case (uintB)cclos_argtype_0_4_key:
                # 4 optional-Argumente, Keyword-Argumente
                OPT_ARG(key_4);
                goto noch_3_opt_args_key;
              unbound_optional_key_4: # Noch 4 optionale Argumente, aber atomp(args)
                pushSTACK(unbound);
              unbound_optional_key_3: # Noch 3 optionale Argumente, aber atomp(args)
                pushSTACK(unbound);
              unbound_optional_key_2: # Noch 2 optionale Argumente, aber atomp(args)
                pushSTACK(unbound);
              unbound_optional_key_1: # Noch 1 optionales Argument, aber atomp(args)
                pushSTACK(unbound);
              unbound_optional_key_0: # Vor den Keywords ist atomp(args)
                if (!nullp(args)) goto fehler_dotted;
                goto apply_cclosure_key_noargs;
              case (uintB)cclos_argtype_default:
                # Allgemeine Version
                break;
              default: NOTREACHED
              #undef OPT_ARG
              #undef REQ_ARG
            }
          # Nun die allgemeine Version:
          { var uintL req_anz = TheCodevec(codevec)->ccv_numreq; # Anzahl required Parameter
            var uintL opt_anz = TheCodevec(codevec)->ccv_numopt; # Anzahl optionale Parameter
            var uintB flags = TheCodevec(codevec)->ccv_flags; # Flags
            # Platz auf dem STACK reservieren:
            get_space_on_STACK(sizeof(object) * (req_anz+opt_anz));
            # required Parameter auswerten und in den Stack ablegen:
            { var uintC count;
              dotimesC(count,req_anz,
                { if (atomp(args)) goto fehler_zuwenig; # Argumentliste zu Ende?
                  pushSTACK(Cdr(args)); # restliche Argumentliste
                  eval(Car(args)); # nächstes Argument auswerten
                  args = STACK_0; STACK_0 = value1; # und in den Stack
                });
            }
            # optionale Parameter auswerten und in den Stack ablegen:
            { var uintC count = opt_anz;
              loop
                { if (atomp(args)) break; # Argumentliste zu Ende?
                  if (count==0) goto optionals_ok; # alle optionalen Parameter versorgt?
                  count--;
                  pushSTACK(Cdr(args)); # restliche Argumentliste
                  eval(Car(args)); # nächstes Argument auswerten
                  args = STACK_0; STACK_0 = value1; # und in den Stack
                }
              # Argumentliste beendet.
              if (!nullp(args)) goto fehler_dotted;
              # Alle weiteren count optionalen Parameter bekommen den "Wert"
              # #<UNBOUND>, der &REST-Parameter den Wert NIL,
              # die Keyword-Parameter den Wert #<UNBOUND> :
              dotimesC(count,count, { pushSTACK(unbound); } );
            }
            closure = *closure_; codevec = TheCclosure(closure)->clos_codevec;
            if (flags & bit(0)) # &REST-Flag?
              { pushSTACK(NIL); } # ja -> mit NIL initialisieren
            if (flags & bit(7)) # &KEY-Flag?
              goto apply_cclosure_key_noargs;
              else
              goto apply_cclosure_nokey_;
            optionals_ok:
            # Rest- und Keyword-Parameter behandeln.
            # args = restliche Argumentliste (noch nicht zu Ende)
            closure = *closure_; codevec = TheCclosure(closure)->clos_codevec;
            if (flags == 0)
              # Closure ohne REST oder KEY -> Argumentliste müsste zu Ende sein
              { goto fehler_zuviel; }
            elif (flags & bit(7)) # Key-Flag?
              # Closure mit Keywords.
              # args = restliche Argumentliste (noch nicht zu Ende)
              # Erst die Keyword-Parameter mit #<UNBOUND> vorbesetzen, dann
              # die restlichen Argumente auswerten und im Stack ablegen, dann
              # die Keywords zuordnen:
              { # evtl. den Rest-Parameter vorbesetzen:
                if (flags & bit(0)) { pushSTACK(unbound); }
                goto apply_cclosure_key;
              }
            else
              goto apply_cclosure_rest_nokey;
          }
          apply_cclosure_key_noargs:
            { var uintC count = TheCodevec(codevec)->ccv_numkey; # Anzahl Keyword-Parameter
              dotimesC(count,count, { pushSTACK(unbound); } ); # mit #<UNBOUND> initialisieren
              interpret_bytecode(closure,codevec,CCV_START_KEY); # Bytecode ab Byte 12 abinterpretieren
            }
            goto done;
          apply_cclosure_key: # Closure mit nur &KEY anspringen:
            {var object* key_args_pointer = args_end_pointer; # Pointer über Keyword-Parameter
             # alle Keyword-Parameter mit #<UNBOUND> vorbesetzen:
             { var uintC count = TheCodevec(codevec)->ccv_numkey;
               dotimesC(count,count, { pushSTACK(unbound); } );
             }
             {var object* rest_args_pointer = args_end_pointer; # Pointer über die restlichen Argumente
              # alle weiteren Argumente auswerten und in den Stack:
              var uintL argcount = 0; # Zähler für die restlichen Argumente
              do { check_STACK();
                   pushSTACK(Cdr(args)); # restliche Argumentliste
                   eval(Car(args)); # nächstes Argument auswerten
                   args = STACK_0; STACK_0 = value1; # und in den Stack
                   argcount++;
                 }
                 while (consp(args));
              # Argumentliste beendet.
              if (!nullp(args)) goto fehler_dotted;
              # Keywords zuordnen, Rest-Parameter bauen
              # und evtl. restliche Argumente wegwerfen:
              closure = match_cclosure_key(*closure_,argcount,key_args_pointer,rest_args_pointer);
              codevec = TheCclosure(closure)->clos_codevec;
              interpret_bytecode(closure,codevec,CCV_START_KEY); # Bytecode ab Byte 12 abinterpretieren
            }}
            goto done;
          apply_cclosure_rest_nokey:
            # Closure mit nur REST, ohne KEY:
            # restlichen Argumente einzeln auswerten, zu einer Liste machen
            # args = restliche Argumentliste (noch nicht zu Ende)
            { pushSTACK(NIL); # bisher ausgewertete restliche Argumente
              pushSTACK(args); # restliche Argumente, unausgewertet
              do { args = STACK_0; STACK_0 = Cdr(args);
                   eval(Car(args)); # nächstes Argument auswerten
                   pushSTACK(value1);
                   # und auf die Liste consen:
                  {var object new_cons = allocate_cons();
                   Car(new_cons) = popSTACK();
                   Cdr(new_cons) = STACK_1;
                   STACK_1 = new_cons;
                 }}
                 while (mconsp(STACK_0));
              args = popSTACK();
              # Liste STACK_0 umdrehen und als REST-Parameter verwenden:
              nreverse(STACK_0);
              # Argumentliste beendet.
              if (!nullp(args)) goto fehler_dotted;
            }
          apply_cclosure_nokey: # Closure ohne &KEY anspringen:
            closure = *closure_; codevec = TheCclosure(closure)->clos_codevec;
          apply_cclosure_nokey_:
            interpret_bytecode(closure,codevec,CCV_START_NONKEY); # Bytecode ab Byte 8 abinterpretieren
          done:
          #if STACKCHECKC
          if (!(STACK == STACKbefore)) # STACK so wie vorher?
            { abort(); } # nein -> ab in den Debugger
          #endif
          skipSTACK(1); # Closure wegwerfen
          unwind(); # EVAL-Frame auflösen
          return; # fertig
          # Gesammelte Fehlermeldungen:
          fehler_zuwenig: # Argumentliste args ist vorzeitig ein Atom
            if (!nullp(args)) goto fehler_dotted;
            setSTACK(STACK = STACKbefore); # STACK aufräumen
            closure = popSTACK();
            fehler_eval_zuwenig(TheCclosure(closure)->clos_name);
          fehler_zuviel: # Argumentliste args ist am Schluss nicht NIL
            if (atomp(args)) goto fehler_dotted;
            setSTACK(STACK = STACKbefore); # STACK aufräumen
            closure = popSTACK();
            fehler_eval_zuviel(TheCclosure(closure)->clos_name);
          fehler_dotted: # Argumentliste args endet mit Atom /= NIL
            setSTACK(STACK = STACKbefore); # STACK aufräumen
            closure = popSTACK();
            fehler_eval_dotted(TheCclosure(closure)->clos_name);
        }
        else
        # closure ist eine interpretierte Closure
        { var object* args_pointer = args_end_pointer; # Pointer über die Argumente
          var uintC args_on_stack = 0; # Anzahl der Argumente
          while (consp(args))
            { pushSTACK(Cdr(args)); # Listenrest retten
              eval(Car(args)); # nächstes Element auswerten
              args = STACK_0; STACK_0 = value1; # Auswertungsergebnis in den STACK
              args_on_stack += 1;
              if (((uintL)~(uintL)0 > ca_limit_1) && (args_on_stack > ca_limit_1))
                goto fehler_zuviel;
            }
          funcall_iclosure(*closure_,args_pointer,args_on_stack);
          skipSTACK(1); # Closure wegwerfen
          unwind(); # EVAL-Frame auflösen
          return; # fertig
        }
    }}

#ifdef DYNAMIC_FFI
# In EVAL: Wendet eine Foreign-Function auf eine Argumentliste an,
# räumt den STACK auf und liefert die Werte.
# eval_ffunction(fun);
# > fun: Funktion, eine Foreign-Function
# > STACK-Aufbau: EVAL-Frame, *APPLYHOOK*, Argumentliste.
# < STACK: aufgeräumt
# < mv_count/mv_space: Werte
# verändert STACK
# can trigger GC
  local Values eval_ffunction(ffun)
    var object ffun;
    { var object args = popSTACK(); # Argumentliste
      skipSTACK(1); # Wert von *APPLYHOOK* vergessen
      # STACK-Aufbau: EVAL-Frame.
      # (ffun arg ...) --> (FFI::FOREIGN-CALL-OUT ffun arg ...)
      check_SP(); check_STACK();
      pushSTACK(ffun); # Foreign-Funktion als 1. Argument
      { var object* args_pointer = args_end_pointer; # Pointer über die Argumente
        var uintC args_on_stack = 1; # Anzahl der Argumente
        while (consp(args))
          { pushSTACK(Cdr(args)); # Listenrest retten
            eval(Car(args)); # nächstes Element auswerten
            args = STACK_0; STACK_0 = value1; # Auswertungsergebnis in den STACK
            args_on_stack += 1;
            if (((uintL)~(uintL)0 > ca_limit_1) && (args_on_stack > ca_limit_1))
              { set_args_end_pointer(args_pointer);
                fehler_eval_zuviel(popSTACK());
              }
          }
        funcall(L(foreign_call_out),args_on_stack);
      }
      unwind(); # EVAL-Frame auflösen
      return; # fertig
    }
#endif


#          ----------------------- A P P L Y -----------------------

# später:
  local Values apply_subr (object fun, uintC args_on_stack, object other_args);
  local Values apply_closure (object fun, uintC args_on_stack, object other_args);

# UP: Wendet eine Funktion auf ihre Argumente an.
# apply(function,args_on_stack,other_args);
# > function: Funktion
# > Argumente: args_on_stack Argumente auf dem STACK,
#              restliche Argumentliste in other_args
# < STACK: aufgeräumt (d.h. STACK wird um args_on_stack erhöht)
# < mv_count/mv_space: Werte
# verändert STACK, kann GC auslösen
  global Values apply (object fun, uintC args_on_stack, object other_args);
  global Values apply(fun,args_on_stack,other_args)
    var object fun;
    var uintC args_on_stack;
    var object other_args;
    { # fun muss ein SUBR oder eine Closure oder ein Cons (LAMBDA ...) sein:
      if (subrp(fun)) # SUBR ?
        { return_Values apply_subr(fun,args_on_stack,other_args); }
      elif (closurep(fun)) # Closure ?
        { return_Values apply_closure(fun,args_on_stack,other_args); }
      elif (symbolp(fun)) # Symbol ?
        # Symbol anwenden: globale Definition Symbol_function(fun) gilt.
        { var object fdef = Symbol_function(fun);
          if (subrp(fdef)) # SUBR -> anwenden
            { return_Values apply_subr(fdef,args_on_stack,other_args); }
          elif (closurep(fdef)) # Closure -> anwenden
            { return_Values apply_closure(fdef,args_on_stack,other_args); }
          elif (orecordp(fdef))
            {
              #ifdef DYNAMIC_FFI
              if (ffunctionp(fdef)) # Foreign-Function ?
                { fun = fdef; goto call_ffunction; }
              #endif
              # FSUBR -> Fehler
              fehler_specialform(S(apply),fun);
            }
          elif (consp(fdef)) # Macro-Cons -> Fehler
            { fehler_macro(S(apply),fun); }
          else
            # wenn kein SUBR, keine Closure, kein FSUBR, kein Cons:
            # Symbol_function(fun) muss #<UNBOUND> sein.
            undef:
            { fehler_undefined(S(apply),fun); }
        }
      elif (funnamep(fun)) # Liste (SETF symbol) ?
        # globale Definition (symbol-function (get-setf-symbol symbol)) gilt.
        { var object symbol = get(Car(Cdr(fun)),S(setf_function)); # (get ... 'SYS::SETF-FUNCTION)
          if (!symbolp(symbol)) # sollte (uninterniertes) Symbol sein
            goto undef; # sonst undefiniert
         {var object fdef = Symbol_function(symbol);
          if (closurep(fdef)) # Closure -> anwenden
            { return_Values apply_closure(fdef,args_on_stack,other_args); }
          elif (subrp(fdef)) # SUBR -> anwenden
            { return_Values apply_subr(fdef,args_on_stack,other_args); }
          #ifdef DYNAMIC_FFI
          elif (ffunctionp(fdef)) # Foreign-Function ?
            { fun = fdef; goto call_ffunction; }
          #endif
          else
            # Solche Funktionsnamen können keine FSUBRs oder Macros bezeichnen.
            # fdef wird vermutlich #<UNBOUND> sein.
            goto undef;
        }}
      #ifdef DYNAMIC_FFI
      elif (ffunctionp(fun)) # Foreign-Function ?
        # (SYS::FOREIGN-CALL-OUT foreign-function . args) aufrufen
        call_ffunction:
        { # Dazu erst die Argumente im Stack um 1 nach unten verschieben.
          var uintC count;
          var object* ptr = &STACK_0;
          dotimesC(count,args_on_stack,
            { *(ptr STACKop -1) = *ptr; ptr skipSTACKop 1; }
            );
          *(ptr STACKop -1) = fun;
          skipSTACK(-1);
          return_Values apply_subr(L(foreign_call_out),args_on_stack+1,other_args);
        }
      #endif
      elif (consp(fun) && eq(Car(fun),S(lambda))) # Cons (LAMBDA ...) ?
        { subr_self = L(apply); fehler_lambda_expression(fun); }
      else
        { pushSTACK(fun); # Wert für Slot DATUM von TYPE-ERROR
          pushSTACK(O(type_designator_function)); # Wert für Slot EXPECTED-TYPE von TYPE-ERROR
          pushSTACK(fun);
          pushSTACK(S(apply));
          fehler(type_error,
                 GETTEXT("~: ~ is not a function name")
                );
        }
    }

# Fehler wegen punktierter Argumentliste
# > name: Name der Funktion
  nonreturning_function(local, fehler_apply_dotted, (object name));
  local void fehler_apply_dotted(name)
    var object name;
    { pushSTACK(name);
      fehler(program_error,
             GETTEXT("APPLY: argument list given to ~ is dotted")
            );
    }

# Fehler wegen zu vielen Argumenten
# > name: Name der Funktion
  nonreturning_function(local, fehler_apply_zuviel, (object name));
  local void fehler_apply_zuviel(name)
    var object name;
    { pushSTACK(name);
      fehler(program_error,
             GETTEXT("APPLY: too many arguments given to ~")
            );
    }

# Fehler wegen zu wenig Argumenten
# > name: Name der Funktion
  nonreturning_function(local, fehler_apply_zuwenig, (object name));
  local void fehler_apply_zuwenig(name)
    var object name;
    { pushSTACK(name);
      fehler(program_error,
             GETTEXT("APPLY: too few arguments given to ~")
            );
    }

# Fehler wegen zu vielen Argumenten für ein SUBR
# > fun: Funktion, ein SUBR
  nonreturning_function(local, fehler_subr_zuviel, (object fun));
  #define fehler_subr_zuviel(fun)  fehler_apply_zuviel(TheSubr(fun)->name)

# Fehler wegen zu wenig Argumenten für ein SUBR
# > fun: Funktion, ein SUBR
  nonreturning_function(local, fehler_subr_zuwenig, (object fun));
  #define fehler_subr_zuwenig(fun)  fehler_apply_zuwenig(TheSubr(fun)->name)

# In APPLY: Wendet ein SUBR auf eine Argumentliste an, räumt den STACK auf
# und liefert die Werte.
# apply_subr(fun,args_on_stack,other_args);
# > fun: Funktion, ein SUBR
# > Argumente: args_on_stack Argumente auf dem STACK,
#              restliche Argumentliste in other_args
# < STACK: aufgeräumt (d.h. STACK wird um args_on_stack erhöht)
# < mv_count/mv_space: Werte
# verändert STACK, kann GC auslösen
  local Values apply_subr(fun,args_on_stack,args)
    var object fun;
    var uintC args_on_stack;
    var object args;
    {
      #if STACKCHECKS
      var object* args_pointer = args_end_pointer STACKop args_on_stack; # Pointer über die Argumente
      #endif
      var object* key_args_pointer; # Pointer über die Keyword-Argumente
      var object* rest_args_pointer; # Pointer über die restlichen Argumente
      var uintL argcount; # Anzahl der restlichen Argumente
      #ifdef DEBUG_EVAL
      if (streamp(Symbol_value(S(funcall_trace_output))))
        { pushSTACK(fun); trace_call(fun,'A','S'); fun = popSTACK(); }
      #endif
      # Argumente in den STACK legen:
      # erst ein Dispatch für die wichtigsten Fälle:
      switch (TheSubr(fun)->argtype)
        { # Macro für ein required-Argument:
          #define REQ_ARG()  \
            { if (args_on_stack>0) { args_on_stack--; }                      \
              elif (consp(args)) { pushSTACK(Car(args)); args = Cdr(args); } \
              else goto fehler_zuwenig;                                      \
            }
          # Macro für das n-letzte optional-Argument:
          #define OPT_ARG(n)  \
            { if (args_on_stack>0) { args_on_stack--; }                      \
              elif (consp(args)) { pushSTACK(Car(args)); args = Cdr(args); } \
              else goto unbound_optional_##n;                                \
            }
          case (uintW)subr_argtype_6_0:
            # SUBR mit 6 required-Argumenten
            REQ_ARG();
          case (uintW)subr_argtype_5_0:
            # SUBR mit 5 required-Argumenten
            REQ_ARG();
          case (uintW)subr_argtype_4_0:
            # SUBR mit 4 required-Argumenten
            REQ_ARG();
          case (uintW)subr_argtype_3_0:
            # SUBR mit 3 required-Argumenten
            REQ_ARG();
          case (uintW)subr_argtype_2_0:
            # SUBR mit 2 required-Argumenten
            REQ_ARG();
          case (uintW)subr_argtype_1_0:
            # SUBR mit 1 required-Argument
            REQ_ARG();
          case (uintW)subr_argtype_0_0:
            # SUBR ohne Argumente
            if ((args_on_stack>0) || consp(args)) goto fehler_zuviel;
            goto apply_subr_norest;
          case (uintW)subr_argtype_4_1:
            # SUBR mit 4 required-Argumenten und 1 optional-Argument
            REQ_ARG();
          case (uintW)subr_argtype_3_1:
            # SUBR mit 3 required-Argumenten und 1 optional-Argument
            REQ_ARG();
          case (uintW)subr_argtype_2_1:
            # SUBR mit 2 required-Argumenten und 1 optional-Argument
            REQ_ARG();
          case (uintW)subr_argtype_1_1:
            # SUBR mit 1 required-Argument und 1 optional-Argument
            REQ_ARG();
          case (uintW)subr_argtype_0_1:
            # SUBR mit 1 optional-Argument
            OPT_ARG(1);
            if ((args_on_stack>0) || consp(args)) goto fehler_zuviel;
            goto apply_subr_norest;
          case (uintW)subr_argtype_2_2:
            # SUBR mit 2 required-Argumenten und 2 optional-Argumenten
            REQ_ARG();
          case (uintW)subr_argtype_1_2:
            # SUBR mit 1 required-Argument und 2 optional-Argumenten
            REQ_ARG();
          case (uintW)subr_argtype_0_2:
            # SUBR mit 2 optional-Argumenten
            OPT_ARG(2);
            OPT_ARG(1);
            if ((args_on_stack>0) || consp(args)) goto fehler_zuviel;
            goto apply_subr_norest;
          case (uintW)subr_argtype_2_3:
            # SUBR mit 2 required-Argumenten und 3 optional-Argumenten
            REQ_ARG();
          case (uintW)subr_argtype_1_3:
            # SUBR mit 1 required-Argument und 3 optional-Argumenten
            REQ_ARG();
          case (uintW)subr_argtype_0_3:
            # SUBR mit 3 optional-Argumenten
            OPT_ARG(3);
            OPT_ARG(2);
            OPT_ARG(1);
            if ((args_on_stack>0) || consp(args)) goto fehler_zuviel;
            goto apply_subr_norest;
          case (uintW)subr_argtype_0_5:
            # SUBR mit 5 optional-Argumenten
            OPT_ARG(5);
          case (uintW)subr_argtype_0_4:
            # SUBR mit 4 optional-Argumenten
            OPT_ARG(4);
            OPT_ARG(3);
            OPT_ARG(2);
            OPT_ARG(1);
            if ((args_on_stack>0) || consp(args)) goto fehler_zuviel;
            goto apply_subr_norest;
          unbound_optional_5: # Noch 5 optionale Argumente, aber args_on_stack=0 und atomp(args)
            pushSTACK(unbound);
          unbound_optional_4: # Noch 4 optionale Argumente, aber args_on_stack=0 und atomp(args)
            pushSTACK(unbound);
          unbound_optional_3: # Noch 3 optionale Argumente, aber args_on_stack=0 und atomp(args)
            pushSTACK(unbound);
          unbound_optional_2: # Noch 2 optionale Argumente, aber args_on_stack=0 und atomp(args)
            pushSTACK(unbound);
          unbound_optional_1: # Noch 1 optionales Argument, aber args_on_stack=0 und atomp(args)
            pushSTACK(unbound);
            goto apply_subr_norest;
          case (uintW)subr_argtype_3_0_rest:
            # SUBR mit 3 required-Argumenten und weiteren Argumenten
            REQ_ARG();
          case (uintW)subr_argtype_2_0_rest:
            # SUBR mit 2 required-Argumenten und weiteren Argumenten
            REQ_ARG();
          case (uintW)subr_argtype_1_0_rest:
            # SUBR mit 1 required-Argument und weiteren Argumenten
            REQ_ARG();
          case (uintW)subr_argtype_0_0_rest:
            # SUBR mit weiteren Argumenten
            if (args_on_stack==0)
              goto apply_subr_rest_onlylist;
              else
              goto apply_subr_rest_withlist;
          case (uintW)subr_argtype_4_0_key:
            # SUBR mit 4 required-Argumenten und Keyword-Argumenten
            REQ_ARG();
          case (uintW)subr_argtype_3_0_key:
            # SUBR mit 3 required-Argumenten und Keyword-Argumenten
            REQ_ARG();
          case (uintW)subr_argtype_2_0_key:
            # SUBR mit 2 required-Argumenten und Keyword-Argumenten
            REQ_ARG();
          case (uintW)subr_argtype_1_0_key:
            # SUBR mit 1 required-Argument und Keyword-Argumenten
            REQ_ARG();
          case (uintW)subr_argtype_0_0_key:
            # SUBR mit Keyword-Argumenten
            if ((args_on_stack==0) && atomp(args)) goto unbound_optional_key_0;
            goto apply_subr_key;
          case (uintW)subr_argtype_1_1_key:
            # SUBR mit 1 required-Argument, 1 optional-Argument und Keyword-Argumenten
            REQ_ARG();
          case (uintW)subr_argtype_0_1_key:
            # SUBR mit 1 optional-Argument und Keyword-Argumenten
            OPT_ARG(key_1);
            if ((args_on_stack==0) && atomp(args)) goto unbound_optional_key_0;
            goto apply_subr_key;
          case (uintW)subr_argtype_1_2_key:
            # SUBR mit 1 required-Argument, 2 optional-Argumenten und Keyword-Argumenten
            REQ_ARG();
            OPT_ARG(key_2);
            OPT_ARG(key_1);
            if ((args_on_stack==0) && atomp(args)) goto unbound_optional_key_0;
            goto apply_subr_key;
          unbound_optional_key_2: # Noch 2 optionale Argumente, aber args_on_stack=0 und atomp(args)
            pushSTACK(unbound);
          unbound_optional_key_1: # Noch 1 optionales Argument, aber args_on_stack=0 und atomp(args)
            pushSTACK(unbound);
          unbound_optional_key_0: # Vor den Keywords ist args_on_stack=0 und atomp(args)
            { var uintC count;
              dotimesC(count,TheSubr(fun)->key_anz, { pushSTACK(unbound); } );
            }
            goto apply_subr_norest;
          default: NOTREACHED
          #undef OPT_ARG
          #undef REQ_ARG
        }
      # Nun die allgemeine Version:
      {var uintC key_anz;
       {var uintC req_anz = TheSubr(fun)->req_anz;
        var uintC opt_anz = TheSubr(fun)->opt_anz;
        key_anz = TheSubr(fun)->key_anz;
        if (args_on_stack < req_anz)
          # weniger Argumente da als verlangt
          { req_anz = req_anz - args_on_stack; # soviele müssen noch auf den STACK
            # Platz auf dem STACK reservieren:
            get_space_on_STACK(sizeof(object) * (uintL)(req_anz + opt_anz + key_anz));
            # required Parameter in den Stack ablegen:
            { var uintC count;
              dotimespC(count,req_anz,
                { if (atomp(args)) { goto fehler_zuwenig; }
                  pushSTACK(Car(args)); # nächstes Argument ablegen
                  args = Cdr(args);
                });
            }
            goto optionals_from_list;
          }
        args_on_stack -= req_anz; # verbleibende Anzahl
        if (args_on_stack < opt_anz)
          # Argumente im Stack reichen nicht für die optionalen
          { opt_anz = opt_anz - args_on_stack; # soviele müssen noch auf den STACK
            # Platz auf dem STACK reservieren:
            get_space_on_STACK(sizeof(object) * (uintL)(opt_anz + key_anz));
            optionals_from_list:
            # optionale Parameter in den Stack ablegen:
            { var uintC count = opt_anz;
              loop
                { if (atomp(args)) break; # Argumentliste zu Ende?
                  if (count==0) goto optionals_ok; # alle optionalen Parameter versorgt?
                  count--;
                  pushSTACK(Car(args)); # nächstes Argument ablegen
                  args = Cdr(args);
                }
              # Argumentliste beendet.
              # Alle weiteren count optionalen Parameter bekommen den "Wert"
              # #<UNBOUND>, auch die Keyword-Parameter:
              dotimesC(count,count + key_anz, { pushSTACK(unbound); } );
              if (TheSubr(fun)->rest_flag == subr_rest) # &REST-Flag?
                # ja -> 0 zusätzliche Argumente:
                { argcount = 0; rest_args_pointer = args_end_pointer;
                  goto apply_subr_rest;
                }
                else
                # nein -> nichts zu tun
                { goto apply_subr_norest; }
            }
            optionals_ok: # optionale Argumente OK, (nichtleere) Liste weiter abarbeiten
            if (TheSubr(fun)->key_flag == subr_nokey)
              # SUBR ohne KEY
              { if (TheSubr(fun)->rest_flag == subr_norest)
                  # SUBR ohne REST oder KEY
                  { fehler_subr_zuviel(fun); } # zuviele Argumente
                  else
                  # SUBR mit nur REST, ohne KEY
                  goto apply_subr_rest_onlylist;
              }
              else
              # SUBR mit KEY
              { key_args_pointer = args_end_pointer;
                { var uintC count;
                  dotimesC(count,key_anz, { pushSTACK(unbound); } );
                }
                rest_args_pointer = args_end_pointer;
                argcount = 0;
                goto key_from_list;
              }
          }
        args_on_stack -= opt_anz; # verbleibende Anzahl
        if (TheSubr(fun)->key_flag == subr_nokey)
          # SUBR ohne KEY
          { if (TheSubr(fun)->rest_flag == subr_norest)
              # SUBR ohne REST oder KEY
              { if ((args_on_stack>0) || consp(args)) # noch Argumente?
                  { fehler_subr_zuviel(fun); }
                goto apply_subr_norest;
              }
              else
              # SUBR mit nur REST, ohne KEY
              goto apply_subr_rest_withlist;
          }
          else
          # SUBR mit Keywords.
          goto apply_subr_key_;
       }
       apply_subr_key:
         { key_anz = TheSubr(fun)->key_anz; }
       apply_subr_key_:
         # restliche Argumente im STACK nach unten schieben und dadurch
         # Platz für die Keyword-Parameter schaffen:
         argcount = args_on_stack;
         get_space_on_STACK(sizeof(object) * (uintL)key_anz);
         {var object* new_args_end_pointer = args_end_pointer STACKop -(uintP)key_anz;
          var object* ptr1 = args_end_pointer;
          var object* ptr2 = new_args_end_pointer;
          var uintC count;
          dotimesC(count,args_on_stack, { BEFORE(ptr2) = BEFORE(ptr1); } );
          key_args_pointer = ptr1;
          rest_args_pointer = ptr2;
          dotimesC(count,key_anz, { NEXT(ptr1) = unbound; } );
          set_args_end_pointer(new_args_end_pointer);
         }
       key_from_list: # restliche Argumente für Keywords aus der Liste nehmen
         while (consp(args))
           { check_STACK(); pushSTACK(Car(args)); # nächstes Argument in den Stack
             args = Cdr(args);
             argcount++;
           }
         # Keywords zuordnen und evtl. restliche Argumente wegwerfen:
         match_subr_key(fun,argcount,key_args_pointer,rest_args_pointer);
         if (TheSubr(fun)->rest_flag != subr_norest)
           # SUBR mit &REST-Flag:
           goto apply_subr_rest;
           else
           # SUBR ohne &REST-Flag:
           goto apply_subr_norest;
      }
      apply_subr_rest_onlylist:
        argcount = 0; rest_args_pointer = args_end_pointer;
        goto rest_from_list;
      apply_subr_rest_withlist:
        argcount = args_on_stack;
        rest_args_pointer = args_end_pointer STACKop argcount;
      rest_from_list: # restliche Argumente aus der Liste nehmen
        while (consp(args))
          { check_STACK(); pushSTACK(Car(args)); # nächstes Argument in den Stack
            args = Cdr(args);
            argcount++;
          }
        if (((uintL)~(uintL)0 > ca_limit_1) && (argcount > ca_limit_1)) # zu viele Argumente?
          { goto fehler_zuviel; }
      apply_subr_rest:
        { if (!nullp(args)) goto fehler_dotted;
          subr_self = fun;
          (*(subr_rest_function*)(TheSubr(fun)->function))
           (argcount,rest_args_pointer);
        }
        goto done;
      apply_subr_norest:
        { if (!nullp(args)) goto fehler_dotted;
          subr_self = fun;
          (*(subr_norest_function*)(TheSubr(fun)->function))();
        }
      done:
      #if STACKCHECKS
      if (!(args_pointer == args_end_pointer)) # Stack aufgeräumt?
        { abort(); } # nein -> ab in den Debugger
      #endif
      return; # fertig
      # Gesammelte Fehlermeldungen:
      fehler_zuwenig: fehler_subr_zuwenig(fun);
      fehler_zuviel: fehler_subr_zuviel(fun);
      fehler_dotted: fehler_apply_dotted(TheSubr(fun)->name);
    }

# Fehler wegen zu vielen Argumenten für eine Closure
# > closure: Funktion, eine Closure
  nonreturning_function(local, fehler_closure_zuviel, (object closure));
  #define fehler_closure_zuviel(closure)  fehler_apply_zuviel(closure)

# Fehler wegen zu wenig Argumenten für eine Closure
# > closure: Funktion, eine Closure
  nonreturning_function(local, fehler_closure_zuwenig, (object closure));
  #define fehler_closure_zuwenig(closure)  fehler_apply_zuwenig(closure)

# In APPLY: Wendet eine Closure auf eine Argumentliste an, räumt den STACK auf
# und liefert die Werte.
# apply_closure(fun,args_on_stack,other_args);
# > fun: Funktion, eine Closure
# > Argumente: args_on_stack Argumente auf dem STACK,
#              restliche Argumentliste in other_args
# < STACK: aufgeräumt (d.h. STACK wird um args_on_stack erhöht)
# < mv_count/mv_space: Werte
# verändert STACK, kann GC auslösen
  local Values apply_closure(closure,args_on_stack,args)
    var object closure;
    var uintC args_on_stack;
    var object args;
    {
      #ifdef DEBUG_EVAL
      if (streamp(Symbol_value(S(funcall_trace_output))))
        { pushSTACK(closure); trace_call(closure,'A','C'); closure = popSTACK(); }
      #endif
      if (simple_bit_vector_p(TheClosure(closure)->clos_codevec))
        # closure ist eine compilierte Closure
        {
          #if STACKCHECKC
          var object* args_pointer = args_end_pointer STACKop args_on_stack; # Pointer über die Argumente
          #endif
          var object codevec = TheCclosure(closure)->clos_codevec; # Code-Vektor
          var object* key_args_pointer; # Pointer über die Keyword-Argumente
          var object* rest_args_pointer; # Pointer über die restlichen Argumente
          var uintL argcount; # Anzahl der restlichen Argumente
          check_SP(); check_STACK();
          # Argumente in den STACK legen:
          # erst ein Dispatch für die wichtigsten Fälle:
          switch (TheCodevec(codevec)->ccv_signature)
            { # Macro für ein required-Argument:
              #define REQ_ARG()  \
                { if (args_on_stack>0) { args_on_stack--; }                      \
                  elif (consp(args)) { pushSTACK(Car(args)); args = Cdr(args); } \
                  else goto fehler_zuwenig;                                      \
                }
              # Macro für das n-letzte optional-Argument:
              #define OPT_ARG(n)  \
                { if (args_on_stack>0) { args_on_stack--; }                      \
                  elif (consp(args)) { pushSTACK(Car(args)); args = Cdr(args); } \
                  else goto unbound_optional_##n;                                \
                }
              case (uintB)cclos_argtype_5_0:
                # 5 required-Argumente
                REQ_ARG();
              case (uintB)cclos_argtype_4_0:
                # 4 required-Argumente
                REQ_ARG();
              case (uintB)cclos_argtype_3_0:
                # 3 required-Argumente
                REQ_ARG();
              case (uintB)cclos_argtype_2_0:
                # 2 required-Argumente
                REQ_ARG();
              case (uintB)cclos_argtype_1_0:
                # 1 required-Argument
                REQ_ARG();
              case (uintB)cclos_argtype_0_0:
                # keine Argumente
                noch_0_opt_args:
                if (args_on_stack>0) goto fehler_zuviel;
                if (!nullp(args))
                  { if (consp(args))
                      goto fehler_zuviel;
                      else
                      goto fehler_dotted;
                  }
                goto apply_cclosure_nokey;
              case (uintB)cclos_argtype_4_1:
                # 4 required-Argumente und 1 optional-Argument
                REQ_ARG();
              case (uintB)cclos_argtype_3_1:
                # 3 required-Argumente und 1 optional-Argument
                REQ_ARG();
              case (uintB)cclos_argtype_2_1:
                # 2 required-Argumente und 1 optional-Argument
                REQ_ARG();
              case (uintB)cclos_argtype_1_1:
                # 1 required-Argument und 1 optional-Argument
                REQ_ARG();
              case (uintB)cclos_argtype_0_1:
                # 1 optional-Argument
                noch_1_opt_args:
                OPT_ARG(1);
                goto noch_0_opt_args;
              case (uintB)cclos_argtype_3_2:
                # 3 required-Argumente und 2 optional-Argumente
                REQ_ARG();
              case (uintB)cclos_argtype_2_2:
                # 2 required-Argumente und 2 optional-Argumente
                REQ_ARG();
              case (uintB)cclos_argtype_1_2:
                # 1 required-Argument und 2 optional-Argumente
                REQ_ARG();
              case (uintB)cclos_argtype_0_2:
                # 2 optional-Argumente
                noch_2_opt_args:
                OPT_ARG(2);
                goto noch_1_opt_args;
              case (uintB)cclos_argtype_2_3:
                # 2 required-Argumente und 3 optional-Argumente
                REQ_ARG();
              case (uintB)cclos_argtype_1_3:
                # 1 required-Argument und 3 optional-Argumente
                REQ_ARG();
              case (uintB)cclos_argtype_0_3:
                # 3 optional-Argumente
                noch_3_opt_args:
                OPT_ARG(3);
                goto noch_2_opt_args;
              case (uintB)cclos_argtype_1_4:
                # 1 required-Argument und 4 optional-Argumente
                REQ_ARG();
              case (uintB)cclos_argtype_0_4:
                # 4 optional-Argumente
                noch_4_opt_args:
                OPT_ARG(4);
                goto noch_3_opt_args;
              case (uintB)cclos_argtype_0_5:
                # 5 optional-Argumente
                OPT_ARG(5);
                goto noch_4_opt_args;
              unbound_optional_5: # Noch 5 optionale Argumente, aber args_on_stack=0 und atomp(args)
                pushSTACK(unbound);
              unbound_optional_4: # Noch 4 optionale Argumente, aber args_on_stack=0 und atomp(args)
                pushSTACK(unbound);
              unbound_optional_3: # Noch 3 optionale Argumente, aber args_on_stack=0 und atomp(args)
                pushSTACK(unbound);
              unbound_optional_2: # Noch 2 optionale Argumente, aber args_on_stack=0 und atomp(args)
                pushSTACK(unbound);
              unbound_optional_1: # Noch 1 optionales Argument, aber args_on_stack=0 und atomp(args)
                pushSTACK(unbound);
                if (!nullp(args)) goto fehler_dotted;
                goto apply_cclosure_nokey;
              case (uintB)cclos_argtype_4_0_rest:
                # 4 required-Argumente, Rest-Parameter
                REQ_ARG();
              case (uintB)cclos_argtype_3_0_rest:
                # 3 required-Argumente, Rest-Parameter
                REQ_ARG();
              case (uintB)cclos_argtype_2_0_rest:
                # 2 required-Argumente, Rest-Parameter
                REQ_ARG();
              case (uintB)cclos_argtype_1_0_rest:
                # 1 required-Argument, Rest-Parameter
                REQ_ARG();
              case (uintB)cclos_argtype_0_0_rest:
                # keine Argumente, Rest-Parameter
                goto apply_cclosure_rest_nokey;
              case (uintB)cclos_argtype_4_0_key:
                # 4 required-Argumente, Keyword-Argumente
                REQ_ARG();
              case (uintB)cclos_argtype_3_0_key:
                # 3 required-Argumente, Keyword-Argumente
                REQ_ARG();
              case (uintB)cclos_argtype_2_0_key:
                # 2 required-Argumente, Keyword-Argumente
                REQ_ARG();
              case (uintB)cclos_argtype_1_0_key:
                # 1 required-Argument, Keyword-Argumente
                REQ_ARG();
                noch_0_opt_args_key:
              case (uintB)cclos_argtype_0_0_key:
                # nur Keyword-Argumente
                if ((args_on_stack==0) && atomp(args)) goto unbound_optional_key_0;
                goto apply_cclosure_key_withlist;
              case (uintB)cclos_argtype_3_1_key:
                # 3 required-Argumente und 1 optional-Argument, Keyword-Argumente
                REQ_ARG();
              case (uintB)cclos_argtype_2_1_key:
                # 2 required-Argumente und 1 optional-Argument, Keyword-Argumente
                REQ_ARG();
              case (uintB)cclos_argtype_1_1_key:
                # 1 required-Argument und 1 optional-Argument, Keyword-Argumente
                REQ_ARG();
              case (uintB)cclos_argtype_0_1_key:
                # 1 optional-Argument, Keyword-Argumente
                noch_1_opt_args_key:
                OPT_ARG(key_1);
                goto noch_0_opt_args_key;
              case (uintB)cclos_argtype_2_2_key:
                # 2 required-Argumente und 2 optional-Argumente, Keyword-Argumente
                REQ_ARG();
              case (uintB)cclos_argtype_1_2_key:
                # 1 required-Argument und 2 optional-Argumente, Keyword-Argumente
                REQ_ARG();
              case (uintB)cclos_argtype_0_2_key:
                # 2 optional-Argumente, Keyword-Argumente
                noch_2_opt_args_key:
                OPT_ARG(key_2);
                goto noch_1_opt_args_key;
              case (uintB)cclos_argtype_1_3_key:
                # 1 required-Argument und 3 optional-Argumente, Keyword-Argumente
                REQ_ARG();
              case (uintB)cclos_argtype_0_3_key:
                # 3 optional-Argumente, Keyword-Argumente
                noch_3_opt_args_key:
                OPT_ARG(key_3);
                goto noch_2_opt_args_key;
              case (uintB)cclos_argtype_0_4_key:
                # 4 optional-Argumente, Keyword-Argumente
                OPT_ARG(key_4);
                goto noch_3_opt_args_key;
              unbound_optional_key_4: # Noch 4 optionale Argumente, aber args_on_stack=0 und atomp(args)
                pushSTACK(unbound);
              unbound_optional_key_3: # Noch 3 optionale Argumente, aber args_on_stack=0 und atomp(args)
                pushSTACK(unbound);
              unbound_optional_key_2: # Noch 2 optionale Argumente, aber args_on_stack=0 und atomp(args)
                pushSTACK(unbound);
              unbound_optional_key_1: # Noch 1 optionales Argument, aber args_on_stack=0 und atomp(args)
                pushSTACK(unbound);
              unbound_optional_key_0: # Vor den Keywords ist args_on_stack=0 und atomp(args)
                if (!nullp(args)) goto fehler_dotted;
                goto apply_cclosure_key_noargs;
              case (uintB)cclos_argtype_default:
                # Allgemeine Version
                break;
              default: NOTREACHED
              #undef OPT_ARG
              #undef REQ_ARG
            }
          # Nun die allgemeine Version:
         {var uintB flags;
          {var uintC req_anz = TheCodevec(codevec)->ccv_numreq; # Anzahl required Parameter
           var uintC opt_anz = TheCodevec(codevec)->ccv_numopt; # Anzahl optionale Parameter
           flags = TheCodevec(codevec)->ccv_flags; # Flags
           if (args_on_stack < req_anz)
             # weniger Argumente da als verlangt
             { req_anz = req_anz - args_on_stack; # soviele müssen noch auf den STACK
               # Platz auf dem STACK reservieren:
               get_space_on_STACK(sizeof(object) * (uintL)(req_anz + opt_anz));
               # required Parameter in den Stack ablegen:
               { var uintC count;
                 dotimespC(count,req_anz,
                   { if (atomp(args)) { goto fehler_zuwenig; }
                     pushSTACK(Car(args)); # nächstes Argument ablegen
                     args = Cdr(args);
                   });
               }
               goto optionals_from_list;
             }
           args_on_stack -= req_anz; # verbleibende Anzahl
           if (args_on_stack < opt_anz)
             # Argumente im Stack reichen nicht für die optionalen
             { opt_anz = opt_anz - args_on_stack; # soviele müssen noch auf den STACK
               # Platz auf dem STACK reservieren:
               get_space_on_STACK(sizeof(object) * (uintL)opt_anz);
               optionals_from_list:
               # optionale Parameter in den Stack ablegen:
               { var uintC count = opt_anz;
                 loop
                   { if (atomp(args)) break; # Argumentliste zu Ende?
                     if (count==0) goto optionals_ok; # alle optionalen Parameter versorgt?
                     count--;
                     pushSTACK(Car(args)); # nächstes Argument ablegen
                     args = Cdr(args);
                   }
                 # Argumentliste beendet.
                 if (!nullp(args)) goto fehler_dotted;
                 # Alle weiteren count optionalen Parameter bekommen den "Wert"
                 # #<UNBOUND>, der &REST-Parameter den Wert NIL,
                 # die Keyword-Parameter den Wert #<UNBOUND> :
                 dotimesC(count,count, { pushSTACK(unbound); } );
               }
               if (flags & bit(0)) # &REST-Flag?
                 { pushSTACK(NIL); } # ja -> mit NIL initialisieren
               if (flags & bit(7)) # &KEY-Flag?
                 goto apply_cclosure_key_noargs;
                 else
                 goto apply_cclosure_nokey;
               optionals_ok:
               # Rest- und Keyword-Parameter behandeln.
               # args = restliche Argumentliste (noch nicht zu Ende)
               if (flags == 0)
                 # Closure ohne REST oder KEY -> Argumentliste müsste zu Ende sein
                 { goto fehler_zuviel; }
               # evtl. den Rest-Parameter füllen:
               if (flags & bit(0))
                 { pushSTACK(args); }
               if (flags & bit(7)) # Key-Flag?
                 # Closure mit Keywords.
                 # args = restliche Argumentliste (noch nicht zu Ende)
                 # Erst die Keyword-Parameter mit #<UNBOUND> vorbesetzen,
                 # dann die restlichen Argumente im Stack ablegen,
                 # dann die Keywords zuordnen:
                 { key_args_pointer = args_end_pointer; # Pointer über Keyword-Parameter
                   # alle Keyword-Parameter mit #<UNBOUND> vorbesetzen:
                   { var uintC count = TheCodevec(codevec)->ccv_numkey;
                     dotimesC(count,count, { pushSTACK(unbound); } );
                   }
                   rest_args_pointer = args_end_pointer; # Pointer über die restlichen Argumente
                   argcount = 0; # Zähler für die restlichen Argumente
                   goto key_from_list;
                 }
                 else
                 # Closure mit nur REST, ohne KEY:
                 goto apply_cclosure_nokey;
             }
           args_on_stack -= opt_anz; # verbleibende Anzahl
           if (flags & bit(7)) # Key-Flag?
             goto apply_cclosure_key_withlist_;
           elif (flags & bit(0))
             goto apply_cclosure_rest_nokey;
           else
             # Closure ohne REST oder KEY
             { if ((args_on_stack>0) || consp(args)) # noch Argumente?
                 goto fehler_zuviel;
               goto apply_cclosure_nokey;
             }
          }
          apply_cclosure_key_noargs:
          { var uintC key_anz = TheCodevec(codevec)->ccv_numkey; # Anzahl Keyword-Parameter
            if (key_anz > 0)
              { get_space_on_STACK(sizeof(object) * (uintL)key_anz);
               {var uintC count;
                dotimespC(count,key_anz, { pushSTACK(unbound); } ); # mit #<UNBOUND> initialisieren
              }}
            goto apply_cclosure_key;
          }
          apply_cclosure_key_withlist:
            { flags = TheCodevec(codevec)->ccv_flags; } # Flags initialisieren!
          apply_cclosure_key_withlist_:
            # Closure mit Keywords
            {var uintC key_anz = TheCodevec(codevec)->ccv_numkey; # Anzahl Keyword-Parameter
             # restliche Argumente im STACK nach unten schieben und dadurch
             # Platz für die Keyword-Parameter (und evtl. Rest-Parameter)
             # schaffen:
             var uintL shift = key_anz;
             if (flags & bit(0)) { shift++; } # evtl. 1 mehr für Rest-Parameter
             argcount = args_on_stack;
             get_space_on_STACK(sizeof(object) * shift);
             {var object* new_args_end_pointer = args_end_pointer STACKop -(uintP)shift;
              var object* ptr1 = args_end_pointer;
              var object* ptr2 = new_args_end_pointer;
              var uintC count;
              dotimesC(count,args_on_stack, { BEFORE(ptr2) = BEFORE(ptr1); } );
              if (flags & bit(0)) { NEXT(ptr1) = args; } # Rest-Parameter (vorläufig)
              key_args_pointer = ptr1;
              rest_args_pointer = ptr2;
              dotimesC(count,key_anz, { NEXT(ptr1) = unbound; } );
              set_args_end_pointer(new_args_end_pointer);
              if (flags & bit(0))
                # Rest-Parameter versorgen, sparsamer als match_cclosure_key das tun würde:
                if (args_on_stack > 0)
                  { var object* ptr3 = new_args_end_pointer;
                    pushSTACK(closure); # Closure retten
                    pushSTACK(args); # args retten
                    dotimespC(count,args_on_stack,
                      { var object new_cons = allocate_cons();
                        Car(new_cons) = BEFORE(ptr3);
                        Cdr(new_cons) = Before(key_args_pointer);
                        Before(key_args_pointer) = new_cons;
                      });
                    args = popSTACK();
                    closure = popSTACK();
                  }
            }}
          key_from_list: # restliche Argumente für Keywords aus der Liste nehmen
            while (consp(args))
              { check_STACK(); pushSTACK(Car(args)); # nächstes Argument in den Stack
                args = Cdr(args);
                argcount++;
              }
            # Argumentliste beendet.
            if (!nullp(args)) goto fehler_dotted;
            # Keywords zuordnen, Rest-Parameter bauen
            # und evtl. restliche Argumente wegwerfen:
            closure = match_cclosure_key(closure,argcount,key_args_pointer,rest_args_pointer);
            codevec = TheCclosure(closure)->clos_codevec;
          apply_cclosure_key:
            interpret_bytecode(closure,codevec,CCV_START_KEY); # Bytecode ab Byte 12 abinterpretieren
          goto done;
         }
          apply_cclosure_rest_nokey:
            # Closure mit nur REST, ohne KEY:
            { # muss noch args_on_stack Argumente aus dem Stack auf args consen:
              pushSTACK(args);
              if (args_on_stack > 0)
                { pushSTACK(closure); # Closure muss gerettet werden
                  dotimespC(args_on_stack,args_on_stack,
                    { var object new_cons = allocate_cons();
                      Cdr(new_cons) = STACK_1;
                      Car(new_cons) = STACK_2; # nächstes Argument draufconsen
                      STACK_2 = new_cons;
                      STACK_1 = STACK_0; skipSTACK(1);
                    });
                  closure = popSTACK(); codevec = TheCclosure(closure)->clos_codevec;
                }
              goto apply_cclosure_nokey;
            }
          apply_cclosure_nokey: # Closure ohne &KEY anspringen:
            interpret_bytecode(closure,codevec,CCV_START_NONKEY); # Bytecode ab Byte 8 abinterpretieren
          done:
          #if STACKCHECKC
          if (!(args_pointer == args_end_pointer)) # Stack aufgeräumt?
            { abort(); } # nein -> ab in den Debugger
          #endif
          return; # fertig
          # Gesammelte Fehlermeldungen:
          fehler_zuwenig: fehler_closure_zuwenig(closure);
          fehler_zuviel: fehler_closure_zuviel(closure);
          fehler_dotted: fehler_apply_dotted(closure);
        }
        else
        # closure ist eine interpretierte Closure
        { # Platz auf dem STACK reservieren:
          get_space_on_STACK(sizeof(object) * llength(args));
          while (consp(args)) # Noch Argumente in der Liste?
            { pushSTACK(Car(args)); # nächstes Element in den STACK
              args = Cdr(args);
              args_on_stack += 1;
              if (((uintL)~(uintL)0 > ca_limit_1) && (args_on_stack > ca_limit_1))
                goto fehler_zuviel;
            }
          funcall_iclosure(closure,args_end_pointer STACKop args_on_stack,args_on_stack);
        }
    }


#        ----------------------- F U N C A L L -----------------------

# später:
  local Values funcall_subr (object fun, uintC args_on_stack);
  local Values funcall_closure (object fun, uintC args_on_stack);

# UP: Wendet eine Funktion auf ihre Argumente an.
# funcall(function,argcount);
# > function: Funktion
# > Argumente: argcount Argumente auf dem STACK
# < STACK: aufgeräumt (d.h. STACK wird um argcount erhöht)
# < mv_count/mv_space: Werte
# verändert STACK, kann GC auslösen
  global Values funcall (object fun, uintC argcount);
  global Values funcall(fun,args_on_stack)
    var object fun;
    var uintC args_on_stack;
    { # fun muss ein SUBR oder eine Closure oder ein Cons (LAMBDA ...) sein:
      if (subrp(fun)) # SUBR ?
        { return_Values funcall_subr(fun,args_on_stack); }
      elif (closurep(fun)) # Closure ?
        { return_Values funcall_closure(fun,args_on_stack); }
      elif (symbolp(fun)) # Symbol ?
        # Symbol anwenden: globale Definition Symbol_function(fun) gilt.
        { var object fdef = Symbol_function(fun);
          if (subrp(fdef)) # SUBR -> anwenden
            { return_Values funcall_subr(fdef,args_on_stack); }
          elif (closurep(fdef)) # Closure -> anwenden
            { return_Values funcall_closure(fdef,args_on_stack); }
          elif (orecordp(fdef))
            {
              #ifdef DYNAMIC_FFI
              if (ffunctionp(fdef)) # Foreign-Function ?
                { fun = fdef; goto call_ffunction; }
              #endif
              # FSUBR -> Fehler
              fehler_specialform(S(funcall),fun);
            }
          elif (consp(fdef)) # Macro-Cons -> Fehler
            { fehler_macro(S(funcall),fun); }
          else
            # wenn kein SUBR, keine Closure, kein FSUBR, kein Cons:
            # Symbol_function(fun) muss #<UNBOUND> sein.
            undef:
            { fehler_undefined(S(funcall),fun); }
        }
      elif (funnamep(fun)) # Liste (SETF symbol) ?
        # globale Definition (symbol-function (get-setf-symbol symbol)) gilt.
        { var object symbol = get(Car(Cdr(fun)),S(setf_function)); # (get ... 'SYS::SETF-FUNCTION)
          if (!symbolp(symbol)) # sollte (uninterniertes) Symbol sein
            goto undef; # sonst undefiniert
         {var object fdef = Symbol_function(symbol);
          if (closurep(fdef)) # Closure -> anwenden
            { return_Values funcall_closure(fdef,args_on_stack); }
          elif (subrp(fdef)) # SUBR -> anwenden
            { return_Values funcall_subr(fdef,args_on_stack); }
          #ifdef DYNAMIC_FFI
          elif (ffunctionp(fdef)) # Foreign-Function ?
            { fun = fdef; goto call_ffunction; }
          #endif
          else
            # Solche Funktionsnamen können keine FSUBRs oder Macros bezeichnen.
            # fdef wird vermutlich #<UNBOUND> sein.
            goto undef;
        }}
      #ifdef DYNAMIC_FFI
      elif (ffunctionp(fun)) # Foreign-Function ?
        # (SYS::FOREIGN-CALL-OUT foreign-function . args) aufrufen
        call_ffunction:
        { # Dazu erst die Argumente im Stack um 1 nach unten verschieben.
          var uintC count;
          var object* ptr = &STACK_0;
          dotimesC(count,args_on_stack,
            { *(ptr STACKop -1) = *ptr; ptr skipSTACKop 1; }
            );
          *(ptr STACKop -1) = fun;
          skipSTACK(-1);
          return_Values funcall_subr(L(foreign_call_out),args_on_stack+1);
        }
      #endif
      elif (consp(fun) && eq(Car(fun),S(lambda))) # Cons (LAMBDA ...) ?
        { subr_self = L(funcall); fehler_lambda_expression(fun); }
      else
        { pushSTACK(fun); # Wert für Slot DATUM von TYPE-ERROR
          pushSTACK(O(type_designator_function)); # Wert für Slot EXPECTED-TYPE von TYPE-ERROR
          pushSTACK(fun);
          pushSTACK(S(funcall));
          fehler(type_error,
                 GETTEXT("~: ~ is not a function name")
                );
        }
    }

# In FUNCALL: Wendet ein SUBR auf Argumente an, räumt den STACK auf
# und liefert die Werte.
# funcall_subr(fun,args_on_stack);
# > fun: Funktion, ein SUBR
# > Argumente: args_on_stack Argumente auf dem STACK
# < STACK: aufgeräumt (d.h. STACK wird um args_on_stack erhöht)
# < mv_count/mv_space: Werte
# verändert STACK, kann GC auslösen
  local Values funcall_subr(fun,args_on_stack)
    var object fun;
    var uintC args_on_stack;
    {
      #if STACKCHECKS
      var object* args_pointer = args_end_pointer STACKop args_on_stack; # Pointer über die Argumente
      #endif
      var object* key_args_pointer; # Pointer über die Keyword-Argumente
      var object* rest_args_pointer; # Pointer über die restlichen Argumente
      var uintL argcount; # Anzahl der restlichen Argumente
      #ifdef DEBUG_EVAL
      if (streamp(Symbol_value(S(funcall_trace_output))))
        { pushSTACK(fun); trace_call(fun,'F','S'); fun = popSTACK(); }
      #endif
      # Argumente in den STACK legen:
      # erst ein Dispatch für die wichtigsten Fälle:
      switch (TheSubr(fun)->argtype)
        { case (uintW)subr_argtype_0_0:
            # SUBR ohne Argumente
            if (!(args_on_stack==0)) goto fehler_zuviel;
            goto apply_subr_norest;
          case (uintW)subr_argtype_1_0:
            # SUBR mit 1 required-Argument
            if (!(args_on_stack==1)) goto fehler_anzahl;
            goto apply_subr_norest;
          case (uintW)subr_argtype_2_0:
            # SUBR mit 2 required-Argumenten
            if (!(args_on_stack==2)) goto fehler_anzahl;
            goto apply_subr_norest;
          case (uintW)subr_argtype_3_0:
            # SUBR mit 3 required-Argumenten
            if (!(args_on_stack==3)) goto fehler_anzahl;
            goto apply_subr_norest;
          case (uintW)subr_argtype_4_0:
            # SUBR mit 4 required-Argumenten
            if (!(args_on_stack==4)) goto fehler_anzahl;
            goto apply_subr_norest;
          case (uintW)subr_argtype_5_0:
            # SUBR mit 5 required-Argumenten
            if (!(args_on_stack==5)) goto fehler_anzahl;
            goto apply_subr_norest;
          case (uintW)subr_argtype_6_0:
            # SUBR mit 6 required-Argumenten
            if (!(args_on_stack==6)) goto fehler_anzahl;
            goto apply_subr_norest;
          case (uintW)subr_argtype_0_1:
            # SUBR mit 1 optional-Argument
            if (args_on_stack==1) goto apply_subr_norest;
            elif (args_on_stack>1) goto fehler_zuviel;
            else { pushSTACK(unbound); goto apply_subr_norest; }
          case (uintW)subr_argtype_1_1:
            # SUBR mit 1 required-Argument und 1 optional-Argument
            if (args_on_stack==2) goto apply_subr_norest;
            elif (args_on_stack>2) goto fehler_zuviel;
            elif (args_on_stack==0) goto fehler_zuwenig;
            else { pushSTACK(unbound); goto apply_subr_norest; }
          case (uintW)subr_argtype_2_1:
            # SUBR mit 2 required-Argumenten und 1 optional-Argument
            if (args_on_stack==3) goto apply_subr_norest;
            elif (args_on_stack>3) goto fehler_zuviel;
            elif (args_on_stack<2) goto fehler_zuwenig;
            else { pushSTACK(unbound); goto apply_subr_norest; }
          case (uintW)subr_argtype_3_1:
            # SUBR mit 3 required-Argumenten und 1 optional-Argument
            if (args_on_stack==4) goto apply_subr_norest;
            elif (args_on_stack>4) goto fehler_zuviel;
            elif (args_on_stack<3) goto fehler_zuwenig;
            else { pushSTACK(unbound); goto apply_subr_norest; }
          case (uintW)subr_argtype_4_1:
            # SUBR mit 4 required-Argumenten und 1 optional-Argument
            if (args_on_stack==5) goto apply_subr_norest;
            elif (args_on_stack>5) goto fehler_zuviel;
            elif (args_on_stack<4) goto fehler_zuwenig;
            else { pushSTACK(unbound); goto apply_subr_norest; }
          case (uintW)subr_argtype_0_2:
            # SUBR mit 2 optional-Argumenten
            switch (args_on_stack)
              { case 0: pushSTACK(unbound);
                case 1: pushSTACK(unbound);
                case 2: goto apply_subr_norest;
                default: goto fehler_zuviel;
              }
          case (uintW)subr_argtype_1_2:
            # SUBR mit 1 required-Argument und 2 optional-Argumenten
            switch (args_on_stack)
              { case 0: goto fehler_zuwenig;
                case 1: pushSTACK(unbound);
                case 2: pushSTACK(unbound);
                case 3: goto apply_subr_norest;
                default: goto fehler_zuviel;
              }
          case (uintW)subr_argtype_2_2:
            # SUBR mit 2 required-Argumenten und 2 optional-Argumenten
            switch (args_on_stack)
              { case 0: goto fehler_zuwenig;
                case 1: goto fehler_zuwenig;
                case 2: pushSTACK(unbound);
                case 3: pushSTACK(unbound);
                case 4: goto apply_subr_norest;
                default: goto fehler_zuviel;
              }
          case (uintW)subr_argtype_0_3:
            # SUBR mit 3 optional-Argumenten
            switch (args_on_stack)
              { case 0: pushSTACK(unbound);
                case 1: pushSTACK(unbound);
                case 2: pushSTACK(unbound);
                case 3: goto apply_subr_norest;
                default: goto fehler_zuviel;
              }
          case (uintW)subr_argtype_1_3:
            # SUBR mit 1 required-Argument und 3 optional-Argumenten
            switch (args_on_stack)
              { case 0: goto fehler_zuwenig;
                case 1: pushSTACK(unbound);
                case 2: pushSTACK(unbound);
                case 3: pushSTACK(unbound);
                case 4: goto apply_subr_norest;
                default: goto fehler_zuviel;
              }
          case (uintW)subr_argtype_2_3:
            # SUBR mit 2 required-Argumenten und 3 optional-Argumenten
            switch (args_on_stack)
              { case 0: goto fehler_zuwenig;
                case 1: goto fehler_zuwenig;
                case 2: pushSTACK(unbound);
                case 3: pushSTACK(unbound);
                case 4: pushSTACK(unbound);
                case 5: goto apply_subr_norest;
                default: goto fehler_zuviel;
              }
          case (uintW)subr_argtype_0_4:
            # SUBR mit 4 optional-Argumenten
            switch (args_on_stack)
              { case 0: pushSTACK(unbound);
                case 1: pushSTACK(unbound);
                case 2: pushSTACK(unbound);
                case 3: pushSTACK(unbound);
                case 4: goto apply_subr_norest;
                default: goto fehler_zuviel;
              }
          case (uintW)subr_argtype_0_5:
            # SUBR mit 5 optional-Argumenten
            switch (args_on_stack)
              { case 0: pushSTACK(unbound);
                case 1: pushSTACK(unbound);
                case 2: pushSTACK(unbound);
                case 3: pushSTACK(unbound);
                case 4: pushSTACK(unbound);
                case 5: goto apply_subr_norest;
                default: goto fehler_zuviel;
              }
          case (uintW)subr_argtype_0_0_rest:
            # SUBR mit weiteren Argumenten
            goto apply_subr_rest_ok;
          case (uintW)subr_argtype_1_0_rest:
            # SUBR mit 1 required-Argument und weiteren Argumenten
            if (args_on_stack==0) goto fehler_zuwenig;
            args_on_stack -= 1;
            goto apply_subr_rest_ok;
          case (uintW)subr_argtype_2_0_rest:
            # SUBR mit 2 required-Argumenten und weiteren Argumenten
            if (args_on_stack<2) goto fehler_zuwenig;
            args_on_stack -= 2;
            goto apply_subr_rest_ok;
          case (uintW)subr_argtype_3_0_rest:
            # SUBR mit 3 required-Argumenten und weiteren Argumenten
            if (args_on_stack<3) goto fehler_zuwenig;
            args_on_stack -= 3;
            goto apply_subr_rest_ok;
          case (uintW)subr_argtype_0_0_key:
            # SUBR mit Keyword-Argumenten
            if (args_on_stack==0) goto unbound_optional_key_0;
            else goto apply_subr_key;
          case (uintW)subr_argtype_1_0_key:
            # SUBR mit 1 required-Argument und Keyword-Argumenten
            if (args_on_stack==1) goto unbound_optional_key_0;
            elif (args_on_stack<1) goto fehler_zuwenig;
            else { args_on_stack -= 1; goto apply_subr_key; }
          case (uintW)subr_argtype_2_0_key:
            # SUBR mit 2 required-Argumenten und Keyword-Argumenten
            if (args_on_stack==2) goto unbound_optional_key_0;
            elif (args_on_stack<2) goto fehler_zuwenig;
            else { args_on_stack -= 2; goto apply_subr_key; }
          case (uintW)subr_argtype_3_0_key:
            # SUBR mit 3 required-Argumenten und Keyword-Argumenten
            if (args_on_stack==3) goto unbound_optional_key_0;
            elif (args_on_stack<3) goto fehler_zuwenig;
            else { args_on_stack -= 3; goto apply_subr_key; }
          case (uintW)subr_argtype_4_0_key:
            # SUBR mit 4 required-Argumenten und Keyword-Argumenten
            if (args_on_stack==4) goto unbound_optional_key_0;
            elif (args_on_stack<4) goto fehler_zuwenig;
            else { args_on_stack -= 4; goto apply_subr_key; }
          case (uintW)subr_argtype_0_1_key:
            # SUBR mit 1 optional-Argument und Keyword-Argumenten
            switch (args_on_stack)
              { case 0: goto unbound_optional_key_1;
                case 1: goto unbound_optional_key_0;
                default: args_on_stack -= 1; goto apply_subr_key;
              }
          case (uintW)subr_argtype_1_1_key:
            # SUBR mit 1 required-Argument, 1 optional-Argument und Keyword-Argumenten
            switch (args_on_stack)
              { case 0: goto fehler_zuwenig;
                case 1: goto unbound_optional_key_1;
                case 2: goto unbound_optional_key_0;
                default: args_on_stack -= 2; goto apply_subr_key;
              }
          case (uintW)subr_argtype_1_2_key:
            # SUBR mit 1 required-Argument, 2 optional-Argumenten und Keyword-Argumenten
            switch (args_on_stack)
              { case 0: goto fehler_zuwenig;
                case 1: goto unbound_optional_key_2;
                case 2: goto unbound_optional_key_1;
                case 3: goto unbound_optional_key_0;
                default: args_on_stack -= 3; goto apply_subr_key;
              }
          unbound_optional_key_2: # Noch 2 optionale Argumente, aber args_on_stack=0
            pushSTACK(unbound);
          unbound_optional_key_1: # Noch 1 optionales Argument, aber args_on_stack=0
            pushSTACK(unbound);
          unbound_optional_key_0: # Vor den Keywords ist args_on_stack=0
            { var uintC count;
              dotimesC(count,TheSubr(fun)->key_anz, { pushSTACK(unbound); } );
            }
            goto apply_subr_norest;
          default: NOTREACHED
          #undef OPT_ARG
          #undef REQ_ARG
        }
      # Nun die allgemeine Version:
      {var uintC key_anz;
       {var uintC req_anz = TheSubr(fun)->req_anz;
        var uintC opt_anz = TheSubr(fun)->opt_anz;
        key_anz = TheSubr(fun)->key_anz;
        if (args_on_stack < req_anz)
          # weniger Argumente da als verlangt
          goto fehler_zuwenig;
        args_on_stack -= req_anz; # verbleibende Anzahl
        if (args_on_stack <= opt_anz)
          # Argumente im Stack reichen nicht für die optionalen
          { opt_anz = opt_anz - args_on_stack; # soviele müssen noch auf den STACK
            if (opt_anz + key_anz > 0)
              { # Platz auf dem STACK reservieren:
                get_space_on_STACK(sizeof(object) * (uintL)(opt_anz + key_anz));
                # Alle weiteren count optionalen Parameter bekommen den "Wert"
                # #<UNBOUND>, auch die Keyword-Parameter:
                { var uintC count;
                  dotimespC(count,opt_anz + key_anz, { pushSTACK(unbound); } );
              } }
            if (TheSubr(fun)->rest_flag == subr_rest) # &REST-Flag?
              # ja -> 0 zusätzliche Argumente:
              { argcount = 0; rest_args_pointer = args_end_pointer;
                goto apply_subr_rest;
              }
              else
              # nein -> nichts zu tun
              { goto apply_subr_norest; }
          }
        args_on_stack -= opt_anz; # verbleibende Anzahl (> 0)
        if (TheSubr(fun)->key_flag == subr_nokey)
          # SUBR ohne KEY
          { if (TheSubr(fun)->rest_flag == subr_norest)
              # SUBR ohne REST oder KEY
              { goto fehler_zuviel; } # noch Argumente!
              else
              # SUBR mit nur REST, ohne KEY
              goto apply_subr_rest_ok;
          }
          else
          # SUBR mit Keywords.
          goto apply_subr_key_;
       }
       apply_subr_key:
         { key_anz = TheSubr(fun)->key_anz; }
       apply_subr_key_:
         # restliche Argumente im STACK nach unten schieben und dadurch
         # Platz für die Keyword-Parameter schaffen:
         argcount = args_on_stack; # (> 0)
         get_space_on_STACK(sizeof(object) * (uintL)key_anz);
         {var object* new_args_end_pointer = args_end_pointer STACKop -(uintP)key_anz;
          var object* ptr1 = args_end_pointer;
          var object* ptr2 = new_args_end_pointer;
          var uintC count;
          dotimespC(count,args_on_stack, { BEFORE(ptr2) = BEFORE(ptr1); } );
          key_args_pointer = ptr1;
          rest_args_pointer = ptr2;
          dotimesC(count,key_anz, { NEXT(ptr1) = unbound; } );
          set_args_end_pointer(new_args_end_pointer);
         }
         # Keywords zuordnen und evtl. restliche Argumente wegwerfen:
         match_subr_key(fun,argcount,key_args_pointer,rest_args_pointer);
         if (TheSubr(fun)->rest_flag != subr_norest)
           # SUBR mit &REST-Flag:
           goto apply_subr_rest;
           else
           # SUBR ohne &REST-Flag:
           goto apply_subr_norest;
      }
      apply_subr_rest_ok:
        { argcount = args_on_stack;
          rest_args_pointer = args_end_pointer STACKop argcount;
        }
      apply_subr_rest:
        { subr_self = fun;
          (*(subr_rest_function*)(TheSubr(fun)->function))
           (argcount,rest_args_pointer);
        }
        goto done;
      apply_subr_norest:
        { subr_self = fun;
          (*(subr_norest_function*)(TheSubr(fun)->function))();
        }
      done:
      #if STACKCHECKS
      if (!(args_pointer == args_end_pointer)) # Stack aufgeräumt?
        { abort(); } # nein -> ab in den Debugger
      #endif
      return; # fertig
      # Gesammelte Fehlermeldungen:
      fehler_anzahl:
        if (args_on_stack < TheSubr(fun)->req_anz)
          { goto fehler_zuwenig; } # zu wenig Argumente
          else
          { goto fehler_zuviel; } # zu viele Argumente
      fehler_zuwenig: fehler_subr_zuwenig(fun);
      fehler_zuviel: fehler_subr_zuviel(fun);
    }

# In FUNCALL: Wendet eine Closure auf Argumente an, räumt den STACK auf
# und liefert die Werte.
# funcall_closure(fun,args_on_stack);
# > fun: Funktion, eine Closure
# > Argumente: args_on_stack Argumente auf dem STACK
# < STACK: aufgeräumt (d.h. STACK wird um args_on_stack erhöht)
# < mv_count/mv_space: Werte
# verändert STACK, kann GC auslösen
  local Values funcall_closure(closure,args_on_stack)
    var object closure;
    var uintC args_on_stack;
    {
      #ifdef DEBUG_EVAL
      if (streamp(Symbol_value(S(funcall_trace_output))))
        { pushSTACK(closure); trace_call(closure,'F','C'); closure = popSTACK(); }
      #endif
      if (simple_bit_vector_p(TheClosure(closure)->clos_codevec))
        # closure ist eine compilierte Closure
        {
          #if STACKCHECKC
          var object* args_pointer = args_end_pointer STACKop args_on_stack; # Pointer über die Argumente
          #endif
          var object codevec = TheCclosure(closure)->clos_codevec; # Code-Vektor
          var object* key_args_pointer; # Pointer über die Keyword-Argumente
          var object* rest_args_pointer; # Pointer über die restlichen Argumente
          var uintL argcount; # Anzahl der restlichen Argumente
          check_SP(); check_STACK();
          # Argumente in den STACK legen:
          # erst ein Dispatch für die wichtigsten Fälle:
          switch (TheCodevec(codevec)->ccv_signature)
            { case (uintB)cclos_argtype_0_0:
                # keine Argumente
                if (!(args_on_stack==0)) goto fehler_zuviel;
                goto apply_cclosure_nokey;
              case (uintB)cclos_argtype_1_0:
                # 1 required-Argument
                if (!(args_on_stack==1)) goto fehler_anzahl;
                goto apply_cclosure_nokey;
              case (uintB)cclos_argtype_2_0:
                # 2 required-Argumente
                if (!(args_on_stack==2)) goto fehler_anzahl;
                goto apply_cclosure_nokey;
              case (uintB)cclos_argtype_3_0:
                # 3 required-Argumente
                if (!(args_on_stack==3)) goto fehler_anzahl;
                goto apply_cclosure_nokey;
              case (uintB)cclos_argtype_4_0:
                # 4 required-Argumente
                if (!(args_on_stack==4)) goto fehler_anzahl;
                goto apply_cclosure_nokey;
              case (uintB)cclos_argtype_5_0:
                # 5 required-Argumente
                if (!(args_on_stack==5)) goto fehler_anzahl;
                goto apply_cclosure_nokey;
              case (uintB)cclos_argtype_0_1:
                # 1 optional-Argument
                if (args_on_stack==1) goto apply_cclosure_nokey;
                elif (args_on_stack>1) goto fehler_zuviel;
                else { pushSTACK(unbound); goto apply_cclosure_nokey; }
              case (uintB)cclos_argtype_1_1:
                # 1 required-Argument und 1 optional-Argument
                if (args_on_stack==2) goto apply_cclosure_nokey;
                elif (args_on_stack>2) goto fehler_zuviel;
                elif (args_on_stack==0) goto fehler_zuwenig;
                else { pushSTACK(unbound); goto apply_cclosure_nokey; }
              case (uintB)cclos_argtype_2_1:
                # 2 required-Argumente und 1 optional-Argument
                if (args_on_stack==3) goto apply_cclosure_nokey;
                elif (args_on_stack>3) goto fehler_zuviel;
                elif (args_on_stack<2) goto fehler_zuwenig;
                else { pushSTACK(unbound); goto apply_cclosure_nokey; }
              case (uintB)cclos_argtype_3_1:
                # 3 required-Argumente und 1 optional-Argument
                if (args_on_stack==4) goto apply_cclosure_nokey;
                elif (args_on_stack>4) goto fehler_zuviel;
                elif (args_on_stack<3) goto fehler_zuwenig;
                else { pushSTACK(unbound); goto apply_cclosure_nokey; }
              case (uintB)cclos_argtype_4_1:
                # 4 required-Argumente und 1 optional-Argument
                if (args_on_stack==5) goto apply_cclosure_nokey;
                elif (args_on_stack>5) goto fehler_zuviel;
                elif (args_on_stack<4) goto fehler_zuwenig;
                else { pushSTACK(unbound); goto apply_cclosure_nokey; }
              case (uintB)cclos_argtype_0_2:
                # 2 optional-Argumente
                switch (args_on_stack)
                  { case 0: pushSTACK(unbound);
                    case 1: pushSTACK(unbound);
                    case 2: goto apply_cclosure_nokey;
                    default: goto fehler_zuviel;
                  }
              case (uintB)cclos_argtype_1_2:
                # 1 required-Argument und 2 optional-Argumente
                switch (args_on_stack)
                  { case 0: goto fehler_zuwenig;
                    case 1: pushSTACK(unbound);
                    case 2: pushSTACK(unbound);
                    case 3: goto apply_cclosure_nokey;
                    default: goto fehler_zuviel;
                  }
              case (uintB)cclos_argtype_2_2:
                # 2 required-Argumente und 2 optional-Argumente
                switch (args_on_stack)
                  { case 0: case 1: goto fehler_zuwenig;
                    case 2: pushSTACK(unbound);
                    case 3: pushSTACK(unbound);
                    case 4: goto apply_cclosure_nokey;
                    default: goto fehler_zuviel;
                  }
              case (uintB)cclos_argtype_3_2:
                # 3 required-Argumente und 2 optional-Argumente
                switch (args_on_stack)
                  { case 0: case 1: case 2: goto fehler_zuwenig;
                    case 3: pushSTACK(unbound);
                    case 4: pushSTACK(unbound);
                    case 5: goto apply_cclosure_nokey;
                    default: goto fehler_zuviel;
                  }
              case (uintB)cclos_argtype_0_3:
                # 3 optional-Argumente
                switch (args_on_stack)
                  { case 0: pushSTACK(unbound);
                    case 1: pushSTACK(unbound);
                    case 2: pushSTACK(unbound);
                    case 3: goto apply_cclosure_nokey;
                    default: goto fehler_zuviel;
                  }
              case (uintB)cclos_argtype_1_3:
                # 1 required-Argument und 3 optional-Argumente
                switch (args_on_stack)
                  { case 0: goto fehler_zuwenig;
                    case 1: pushSTACK(unbound);
                    case 2: pushSTACK(unbound);
                    case 3: pushSTACK(unbound);
                    case 4: goto apply_cclosure_nokey;
                    default: goto fehler_zuviel;
                  }
              case (uintB)cclos_argtype_2_3:
                # 2 required-Argumente und 3 optional-Argumente
                switch (args_on_stack)
                  { case 0: case 1: goto fehler_zuwenig;
                    case 2: pushSTACK(unbound);
                    case 3: pushSTACK(unbound);
                    case 4: pushSTACK(unbound);
                    case 5: goto apply_cclosure_nokey;
                    default: goto fehler_zuviel;
                  }
              case (uintB)cclos_argtype_0_4:
                # 4 optional-Argumente
                switch (args_on_stack)
                  { case 0: pushSTACK(unbound);
                    case 1: pushSTACK(unbound);
                    case 2: pushSTACK(unbound);
                    case 3: pushSTACK(unbound);
                    case 4: goto apply_cclosure_nokey;
                    default: goto fehler_zuviel;
                  }
              case (uintB)cclos_argtype_1_4:
                # 1 required-Argument und 4 optional-Argumente
                switch (args_on_stack)
                  { case 0: goto fehler_zuwenig;
                    case 1: pushSTACK(unbound);
                    case 2: pushSTACK(unbound);
                    case 3: pushSTACK(unbound);
                    case 4: pushSTACK(unbound);
                    case 5: goto apply_cclosure_nokey;
                    default: goto fehler_zuviel;
                  }
              case (uintB)cclos_argtype_0_5:
                # 5 optional-Argumente
                switch (args_on_stack)
                  { case 0: pushSTACK(unbound);
                    case 1: pushSTACK(unbound);
                    case 2: pushSTACK(unbound);
                    case 3: pushSTACK(unbound);
                    case 4: pushSTACK(unbound);
                    case 5: goto apply_cclosure_nokey;
                    default: goto fehler_zuviel;
                  }
              case (uintB)cclos_argtype_0_0_rest:
                # keine Argumente, Rest-Parameter
                goto apply_cclosure_rest_nokey;
              case (uintB)cclos_argtype_1_0_rest:
                # 1 required-Argument, Rest-Parameter
                if (args_on_stack==0) goto fehler_zuwenig;
                args_on_stack -= 1;
                goto apply_cclosure_rest_nokey;
              case (uintB)cclos_argtype_2_0_rest:
                # 2 required-Argumente, Rest-Parameter
                if (args_on_stack<2) goto fehler_zuwenig;
                args_on_stack -= 2;
                goto apply_cclosure_rest_nokey;
              case (uintB)cclos_argtype_3_0_rest:
                # 3 required-Argumente, Rest-Parameter
                if (args_on_stack<3) goto fehler_zuwenig;
                args_on_stack -= 3;
                goto apply_cclosure_rest_nokey;
              case (uintB)cclos_argtype_4_0_rest:
                # 4 required-Argumente, Rest-Parameter
                if (args_on_stack<4) goto fehler_zuwenig;
                args_on_stack -= 4;
                goto apply_cclosure_rest_nokey;
              case (uintB)cclos_argtype_0_0_key:
                # nur Keyword-Argumente
                if (args_on_stack==0) goto unbound_optional_key_0;
                else goto apply_cclosure_key_withargs;
              case (uintB)cclos_argtype_1_0_key:
                # 1 required-Argument, Keyword-Argumente
                if (args_on_stack==1) goto unbound_optional_key_0;
                elif (args_on_stack<1) goto fehler_zuwenig;
                else { args_on_stack -= 1; goto apply_cclosure_key_withargs; }
              case (uintB)cclos_argtype_2_0_key:
                # 2 required-Argumente, Keyword-Argumente
                if (args_on_stack==2) goto unbound_optional_key_0;
                elif (args_on_stack<2) goto fehler_zuwenig;
                else { args_on_stack -= 2; goto apply_cclosure_key_withargs; }
              case (uintB)cclos_argtype_3_0_key:
                # 3 required-Argumente, Keyword-Argumente
                if (args_on_stack==3) goto unbound_optional_key_0;
                elif (args_on_stack<3) goto fehler_zuwenig;
                else { args_on_stack -= 3; goto apply_cclosure_key_withargs; }
              case (uintB)cclos_argtype_4_0_key:
                # 4 required-Argumente, Keyword-Argumente
                if (args_on_stack==4) goto unbound_optional_key_0;
                elif (args_on_stack<4) goto fehler_zuwenig;
                else { args_on_stack -= 4; goto apply_cclosure_key_withargs; }
              case (uintB)cclos_argtype_0_1_key:
                # 1 optional-Argument, Keyword-Argumente
                switch (args_on_stack)
                  { case 0: goto unbound_optional_key_1;
                    case 1: goto unbound_optional_key_0;
                    default: args_on_stack -= 1; goto apply_cclosure_key_withargs;
                  }
              case (uintB)cclos_argtype_1_1_key:
                # 1 required-Argument und 1 optional-Argument, Keyword-Argumente
                switch (args_on_stack)
                  { case 0: goto fehler_zuwenig;
                    case 1: goto unbound_optional_key_1;
                    case 2: goto unbound_optional_key_0;
                    default: args_on_stack -= 2; goto apply_cclosure_key_withargs;
                  }
              case (uintB)cclos_argtype_2_1_key:
                # 2 required-Argumente und 1 optional-Argument, Keyword-Argumente
                switch (args_on_stack)
                  { case 0: case 1: goto fehler_zuwenig;
                    case 2: goto unbound_optional_key_1;
                    case 3: goto unbound_optional_key_0;
                    default: args_on_stack -= 3; goto apply_cclosure_key_withargs;
                  }
              case (uintB)cclos_argtype_3_1_key:
                # 3 required-Argumente und 1 optional-Argument, Keyword-Argumente
                switch (args_on_stack)
                  { case 0: case 1: case 2: goto fehler_zuwenig;
                    case 3: goto unbound_optional_key_1;
                    case 4: goto unbound_optional_key_0;
                    default: args_on_stack -= 4; goto apply_cclosure_key_withargs;
                  }
              case (uintB)cclos_argtype_0_2_key:
                # 2 optional-Argumente, Keyword-Argumente
                switch (args_on_stack)
                  { case 0: goto unbound_optional_key_2;
                    case 1: goto unbound_optional_key_1;
                    case 2: goto unbound_optional_key_0;
                    default: args_on_stack -= 2; goto apply_cclosure_key_withargs;
                  }
              case (uintB)cclos_argtype_1_2_key:
                # 1 required-Argument und 2 optional-Argumente, Keyword-Argumente
                switch (args_on_stack)
                  { case 0: goto fehler_zuwenig;
                    case 1: goto unbound_optional_key_2;
                    case 2: goto unbound_optional_key_1;
                    case 3: goto unbound_optional_key_0;
                    default: args_on_stack -= 3; goto apply_cclosure_key_withargs;
                  }
              case (uintB)cclos_argtype_2_2_key:
                # 2 required-Argumente und 2 optional-Argumente, Keyword-Argumente
                switch (args_on_stack)
                  { case 0: case 1: goto fehler_zuwenig;
                    case 2: goto unbound_optional_key_2;
                    case 3: goto unbound_optional_key_1;
                    case 4: goto unbound_optional_key_0;
                    default: args_on_stack -= 4; goto apply_cclosure_key_withargs;
                  }
              case (uintB)cclos_argtype_0_3_key:
                # 3 optional-Argumente, Keyword-Argumente
                switch (args_on_stack)
                  { case 0: goto unbound_optional_key_3;
                    case 1: goto unbound_optional_key_2;
                    case 2: goto unbound_optional_key_1;
                    case 3: goto unbound_optional_key_0;
                    default: args_on_stack -= 3; goto apply_cclosure_key_withargs;
                  }
              case (uintB)cclos_argtype_1_3_key:
                # 1 required-Argument und 3 optional-Argumente, Keyword-Argumente
                switch (args_on_stack)
                  { case 0: goto fehler_zuwenig;
                    case 1: goto unbound_optional_key_3;
                    case 2: goto unbound_optional_key_2;
                    case 3: goto unbound_optional_key_1;
                    case 4: goto unbound_optional_key_0;
                    default: args_on_stack -= 4; goto apply_cclosure_key_withargs;
                  }
              case (uintB)cclos_argtype_0_4_key:
                # 4 optional-Argumente, Keyword-Argumente
                switch (args_on_stack)
                  { case 0: goto unbound_optional_key_4;
                    case 1: goto unbound_optional_key_3;
                    case 2: goto unbound_optional_key_2;
                    case 3: goto unbound_optional_key_1;
                    case 4: goto unbound_optional_key_0;
                    default: args_on_stack -= 4; goto apply_cclosure_key_withargs;
                  }
              unbound_optional_key_4: # Noch 4 optionale Argumente, aber args_on_stack=0
                pushSTACK(unbound);
              unbound_optional_key_3: # Noch 3 optionale Argumente, aber args_on_stack=0
                pushSTACK(unbound);
              unbound_optional_key_2: # Noch 2 optionale Argumente, aber args_on_stack=0
                pushSTACK(unbound);
              unbound_optional_key_1: # Noch 1 optionales Argument, aber args_on_stack=0
                pushSTACK(unbound);
              unbound_optional_key_0: # Vor den Keywords ist args_on_stack=0
                goto apply_cclosure_key_noargs;
              case (uintB)cclos_argtype_default:
                # Allgemeine Version
                break;
              default: NOTREACHED
            }
          # Nun die allgemeine Version:
         {var uintB flags;
          {var uintC req_anz = TheCodevec(codevec)->ccv_numreq; # Anzahl required Parameter
           var uintC opt_anz = TheCodevec(codevec)->ccv_numopt; # Anzahl optionale Parameter
           flags = TheCodevec(codevec)->ccv_flags; # Flags
           if (args_on_stack < req_anz)
             # weniger Argumente da als verlangt
             { goto fehler_zuwenig; }
           args_on_stack -= req_anz; # verbleibende Anzahl
           if (args_on_stack <= opt_anz)
             # Argumente im Stack reichen nicht für die optionalen
             { opt_anz = opt_anz - args_on_stack; # soviele müssen noch auf den STACK
               if (opt_anz > 0)
                 { # Platz auf dem STACK reservieren:
                   get_space_on_STACK(sizeof(object) * (uintL)opt_anz);
                   # Alle weiteren count optionalen Parameter bekommen den "Wert"
                   # #<UNBOUND>, der &REST-Parameter den Wert NIL,
                   # die Keyword-Parameter den Wert #<UNBOUND> :
                   { var uintC count;
                     dotimespC(count,opt_anz, { pushSTACK(unbound); } );
                 } }
               if (flags & bit(0)) # &REST-Flag?
                 { pushSTACK(NIL); } # ja -> mit NIL initialisieren
               if (flags & bit(7)) # &KEY-Flag?
                 goto apply_cclosure_key_noargs;
                 else
                 goto apply_cclosure_nokey;
             }
           args_on_stack -= opt_anz; # verbleibende Anzahl
           if (flags & bit(7)) # Key-Flag?
             goto apply_cclosure_key_withargs_;
           elif (flags & bit(0))
             goto apply_cclosure_rest_nokey;
           else
             # Closure ohne REST oder KEY
             { if (args_on_stack>0) # noch Argumente?
                 goto fehler_zuviel;
               goto apply_cclosure_nokey;
             }
          }
          apply_cclosure_key_noargs:
          { var uintC key_anz = TheCodevec(codevec)->ccv_numkey; # Anzahl Keyword-Parameter
            if (key_anz > 0)
              { get_space_on_STACK(sizeof(object) * (uintL)key_anz);
               {var uintC count;
                dotimespC(count,key_anz, { pushSTACK(unbound); } ); # mit #<UNBOUND> initialisieren
              }}
            goto apply_cclosure_key;
          }
          apply_cclosure_key_withargs:
            { flags = TheCodevec(codevec)->ccv_flags; } # Flags initialisieren!
          apply_cclosure_key_withargs_:
            # Closure mit Keywords
            {var uintC key_anz = TheCodevec(codevec)->ccv_numkey; # Anzahl Keyword-Parameter
             # restliche Argumente im STACK nach unten schieben und dadurch
             # Platz für die Keyword-Parameter (und evtl. Rest-Parameter)
             # schaffen:
             var uintL shift = key_anz;
             if (flags & bit(0)) { shift++; } # evtl. 1 mehr für Rest-Parameter
             argcount = args_on_stack;
             get_space_on_STACK(sizeof(object) * shift);
             {var object* new_args_end_pointer = args_end_pointer STACKop -(uintP)shift;
              var object* ptr1 = args_end_pointer;
              var object* ptr2 = new_args_end_pointer;
              var uintC count;
              dotimesC(count,args_on_stack, { BEFORE(ptr2) = BEFORE(ptr1); } );
              if (flags & bit(0)) { NEXT(ptr1) = unbound; } # Rest-Parameter
              key_args_pointer = ptr1;
              rest_args_pointer = ptr2;
              dotimesC(count,key_anz, { NEXT(ptr1) = unbound; } );
              set_args_end_pointer(new_args_end_pointer);
            }}
            # Keywords zuordnen, Rest-Parameter bauen
            # und evtl. restliche Argumente wegwerfen:
            closure = match_cclosure_key(closure,argcount,key_args_pointer,rest_args_pointer);
            codevec = TheCclosure(closure)->clos_codevec;
          apply_cclosure_key:
            interpret_bytecode(closure,codevec,CCV_START_KEY); # Bytecode ab Byte 12 abinterpretieren
          goto done;
         }
          apply_cclosure_rest_nokey:
            # Closure mit nur REST, ohne KEY:
            { # muss noch args_on_stack Argumente aus dem Stack zusammenconsen:
              pushSTACK(NIL);
              if (args_on_stack > 0)
                { pushSTACK(closure); # Closure muss gerettet werden
                  dotimesC(args_on_stack,args_on_stack,
                    { var object new_cons = allocate_cons();
                      Cdr(new_cons) = STACK_1;
                      Car(new_cons) = STACK_2; # nächstes Argument draufconsen
                      STACK_2 = new_cons;
                      STACK_1 = STACK_0; skipSTACK(1);
                    });
                  closure = popSTACK(); codevec = TheCclosure(closure)->clos_codevec;
            }   }
          apply_cclosure_nokey: # Closure ohne &KEY anspringen:
            interpret_bytecode(closure,codevec,CCV_START_NONKEY); # Bytecode ab Byte 8 abinterpretieren
          done:
          #if STACKCHECKC
          if (!(args_pointer == args_end_pointer)) # Stack aufgeräumt?
            { abort(); } # nein -> ab in den Debugger
          #endif
          return; # fertig
          # Gesammelte Fehlermeldungen:
          fehler_anzahl:
            if (args_on_stack < TheCodevec(codevec)->ccv_numreq)
              { goto fehler_zuwenig; } # zu wenig Argumente
              else
              { goto fehler_zuviel; } # zu viele Argumente
          fehler_zuwenig: fehler_closure_zuwenig(closure);
          fehler_zuviel: fehler_closure_zuviel(closure);
        }
        else
        # closure ist eine interpretierte Closure
        { funcall_iclosure(closure,args_end_pointer STACKop args_on_stack,args_on_stack); }
    }


#      ---------------------- BYTECODE-INTERPRETER ----------------------

# Interpretiert den Bytecode einer compilierten Closure.
# interpret_bytecode_(closure,codeptr,byteptr);
# > closure: compilierte Closure
# > codeptr: ihr Codevektor, ein Simple-Bit-Vector, pointable
# > byteptr: Start-Bytecodepointer
# < mv_count/mv_space: Werte
# verändert STACK, kann GC auslösen
  # Syntax lokaler Labels in GNU-C Assembler-Anweisungen:
  #if defined(GNU) && !defined(NO_ASM)
    # LD(x) definiert Label mit Nummer x
    # LR(x,f) referenziert Label mit Nummer x vorwärts
    # LR(x,b) referenziert Label mit Nummer x rückwärts
    # Der Sichtbarkeitsbereich der Labels ist nur die eine Assembler-Anweisung.
    #if defined(I80386) && !defined(UNIX_NEXTSTEP)
      #ifdef ASM_UNDERSCORE
        #define LD(nr)  CONCAT("LASM%=X",STRING(nr))
        #define LR(nr,fb)  CONCAT("LASM%=X",STRING(nr))
      #else
        #define LD(nr)  CONCAT(".LASM%=X",STRING(nr))
        #define LR(nr,fb)  CONCAT(".LASM%=X",STRING(nr))
      #endif
    #elif defined(ARM)
      #define LD(nr)  CONCAT("LASM%=X",STRING(nr))
      #define LR(nr,fb)  CONCAT("LASM%=X",STRING(nr))
    #else
      #define LD(nr)  STRING(nr)
      #define LR(nr,fb)  CONCAT(STRING(nr),STRING(fb))
    #endif
  #endif
  # Den GNU-C dazu überreden, closure und byteptr in Registern zu halten:
  #ifdef GNU
    #ifdef MC680X0
      #define closure_register  "a2"
      #define byteptr_register  "a3"
    #endif
    #ifdef SPARC
      #define closure_register  "%l0"
      #define byteptr_register  "%l1"
    #endif
    #ifdef I80386
      #if (__GNUC__ >= 2) # Die Namen der Register haben sich verändert
        #define byteptr_register  "%edi"
      #else
        #define byteptr_register  "di"
      #endif
    #endif
    #ifdef ARM
      # Code is better without defining registers for closure and byteptr,
      # says Peter Burwood.
      # not define closure_register  "%r6"
      # not define byteptr_register  "%r7"
      # We have assembler macros below, but if they are used with gcc-2.7.2.1,
      # (setf cdddr) is miscompiled. So here we temporarily disable them.
      #ifndef NO_ASM
        #define NO_ASM
      #endif
    #endif
    #ifdef CONVEX
      #define closure_register "a5"
      #define byteptr_register "a4"
    #endif
    #ifdef DECALPHA
      #define byteptr_register  "$14"
    #endif
    #ifdef WIDE_SOFT
      # Ein `object' passt nicht in ein einzelnes Register, GCC ist überfordert.
      #undef closure_register
    #endif
  #endif
  #ifndef closure_register
    #define closure_in  closure
  #endif
  #ifndef byteptr_register
    #define byteptr_in  byteptr
  #endif
  local Values interpret_bytecode_(closure_in,codeptr,byteptr_in)
    var object closure_in;
    var Sbvector codeptr;
    var const uintB* byteptr_in;
    { # Argument closure im Register unterbringen:
      #ifdef closure_register
      var object closure __asm__(closure_register);
      closure = closure_in;
      #endif
     {# Argument byteptr im Register unterbringen:
      #ifdef byteptr_register
      var const uintB* byteptr __asm__(byteptr_register);
      byteptr = byteptr_in;
      #endif
      #ifdef DEBUG_EVAL
      if (streamp(Symbol_value(S(funcall_trace_output))))
        { pushSTACK(closure); trace_call(closure,'B','C'); closure = popSTACK(); }
      #endif
      {# Closure im STACK unterbringen, unter die Argumente:
       var object* closureptr = (pushSTACK(closure), &STACK_0);
       #ifndef FAST_SP
         # Hat man keinen schnellen SP-Zugriff, muss man einen extra Pointer
         # einführen:
         var uintL private_SP_length =
           (uintL)(((Codevec)codeptr)->ccv_spdepth_1)
           + jmpbufsize * (uintL)(((Codevec)codeptr)->ccv_spdepth_jmpbufsize);
         var DYNAMIC_ARRAY(private_SP_space,SPint,private_SP_length);
         var SPint* private_SP = &private_SP_space[private_SP_length];
         #undef SP_
         #undef _SP_
         #undef skipSP
         #undef pushSP
         #undef popSP
         #define SP_(n)  (private_SP[n])
         #define _SP_(n)  &SP_(n)
         #define skipSP(n)  (private_SP += (n))
         #define pushSP(item)  (*--private_SP = (item))
         #define popSP(item_zuweisung)  (item_zuweisung *private_SP++)
       #endif
       # var JMPBUF_on_SP(name);  alloziert einen sp_jmp_buf im SP.
       # FREE_JMPBUF_on_SP();  dealloziert ihn wieder.
       # finish_entry_frame_1(frametype,returner,reentry_statement);  ist wie
       # finish_entry_frame(frametype,returner,,reentry_statement);  nur dass
       # auch private_SP gerettet wird.
       #ifndef FAST_SP
         #define JMPBUF_on_SP(name)  \
           sp_jmp_buf* name = (sp_jmp_buf*)(private_SP -= jmpbufsize);
         #define FREE_JMPBUF_on_SP()  \
           private_SP += jmpbufsize;
         #define finish_entry_frame_1(frametype,returner,reentry_statement)  \
           finish_entry_frame(frametype,&!*returner, # Beim Eintritt: returner = private_SP      \
             returner = (sp_jmp_buf*) , # returner wird beim Rücksprung wieder gesetzt           \
             { private_SP = (SPint*)returner; reentry_statement } # und private_SP rekonstruiert \
             )
       #else
         #ifdef SP_DOWN
           #define JMPBUF_on_SP(name)  \
             sp_jmp_buf* name;                   \
             {var SPint* sp = (SPint*)SP();      \
              sp -= jmpbufsize;                  \
              setSP(sp);                         \
              name = (sp_jmp_buf*)&sp[SPoffset]; \
             }
         #endif
         #ifdef SP_UP
           #define JMPBUF_on_SP(name)  \
             sp_jmp_buf* name;                     \
             {var SPint* sp = (SPint*)SP();        \
              name = (sp_jmp_buf*)&sp[SPoffset+1]; \
              sp += jmpbufsize;                    \
              setSP(sp);                           \
             }
         #endif
         #define FREE_JMPBUF_on_SP()  \
           skipSP(jmpbufsize);
         #define finish_entry_frame_1(frametype,returner,reentry_statement)  \
           finish_entry_frame(frametype,&!*returner,,reentry_statement)
       #endif
       #
       #ifdef FAST_DISPATCH
         static void* const cod_labels[] = {
                                             #define BYTECODE(code)  &&code,
                                             #include "bytecode.c"
                                             #undef BYTECODE
                                           };
       #endif
       #
       # nächstes Byte abzuinterpretieren
       # > mv_count/mv_space: aktuelle Werte
       # > closureptr: Pointer auf die compilierte Closure im Stack
       # > closure: compilierte Closure
       # > codeptr: ihr Codevektor, ein Simple-Bit-Vektor, pointable
       #            (kein LISP-Objekt, aber dennoch GC-gefährdet!)
       # > byteptr: Pointer auf das nächste Byte im Code
       #            (kein LISP-Objekt, aber dennoch GC-gefährdet!)
       next_byte:
        # Fallunterscheidung nach abzuinterpretierendem Byte
        #ifndef FAST_DISPATCH
         switch (*byteptr++)
         #define CASE  case (uintB)
        #else # FAST_DISPATCH
         # Das ist etwa 2% schneller, weil die Index-Überprüfung entfällt.
         goto *cod_labels[*byteptr++];
         #define CASE
         #ifdef FAST_DISPATCH_THREADED
          # Die Sprunganweisung  goto next_byte;  kann man sich auch sparen:
          #define next_byte  *cod_labels[*byteptr++]
         #endif
        #endif
          { # Holen der Operanden:
            #   nächstes Byte:
            #     Bit 7 = 0 --> Bits 6..0 sind der Operand (7 Bits).
            #     Bit 7 = 1 --> Bits 6..0 und nächstes Byte bilden den
            #                   Operanden (15 Bits).
            #                   Bei Sprungdistanzen: Sollte dieser =0 sein, so
            #                   bilden die nächsten 4 Bytes den Operanden
            #                   (32 Bits).
            #
            # Macro B_operand(where);
            # bringt den nächsten Operanden (ein Byte als Unsigned Integer)
            # nach (uintL)where und rückt dabei den Bytecodepointer weiter.
              #define B_operand(where)  \
                { where = *byteptr++; }
            #
            # Macro U_operand(where);
            # bringt den nächsten Operanden (ein Unsigned Integer)
            # nach (uintL)where oder (uintC)where
            # und rückt dabei den Bytecodepointer weiter.
              #define U_operand(where)  \
                { where = *byteptr++; # erstes Byte lesen            \
                  if ((uintB)where & bit(7)) # Bit 7 gesetzt?        \
                    { where &= ~bit(7); # ja -> löschen              \
                      where = where << 8;                            \
                      where |= *byteptr++; # und nächstes Byte lesen \
                }   }
            #if defined(GNU) && defined(MC680X0) && !defined(NO_ASM)
              #undef U_operand
              #define U_operand(where)  \
                __asm__(                 \
                  "moveq #0,%0"   "\n\t" \
                  "moveb %1@+,%0" "\n\t" \
                  "bpl 1f"        "\n\t" \
                  "addb %0,%0"    "\n\t" \
                  "lslw #7,%0"    "\n\t" \
                  "moveb %1@+,%0" "\n"   \
                  "1:"                   \
                  : "=d" (where), "=a" (byteptr) : "1" (byteptr) )
            #endif
            #if defined(GNU) && defined(SPARC) && !defined(NO_ASM)
              #undef U_operand
              #define U_operand(where)  \
                { var uintL dummy;              \
                  __asm__(                      \
                    "ldub [%1],%0"       "\n\t" \
                    "andcc %0,0x80,%%g0" "\n\t" \
                    "be 1f"              "\n\t" \
                    " add %1,1,%1"       "\n\t" \
                    "sll %0,25,%2"       "\n\t" \
                    "ldub [%1],%0"       "\n\t" \
                    "srl %2,17,%2"       "\n\t" \
                    "add %1,1,%1"        "\n\t" \
                    "or %0,%2,%0"        "\n"   \
                    "1:"                        \
                    : "=r" (where), "=r" (byteptr), "=r" (dummy) : "1" (byteptr) : "ccr" ); \
                }
            #endif
            #if defined(GNU) && defined(I80386) && !defined(NO_ASM)
              # Bei manchen Assemblern muss das Ergebnis in %eax liegen:
              #if defined(UNIX_LINUX) || defined(UNIX_NETBSD) || defined(UNIX_FREEBSD) || defined(UNIX_OPENBSD)
                # GNU-Assembler: in beliebigem Register.
                # "testb %edx,%edx" wird als "testb %dl,%dl" assembliert.
                #define OUT_EAX  "=q"
                #define EAX      "%0"
                #define AL       "%0"
              #else
                # sonstiger Assembler: in %eax. "testb %edx,%edx" ist illegal.
                #define OUT_EAX  "=a"
                #define EAX      "%%eax"
                #define AL       "%%al"
              #endif
              #undef U_operand
              #define U_operand(where)  \
                __asm__(                   \
                  "movzbl (%1),"EAX "\n\t" \
                  "incl %1"         "\n\t" \
                  "testb "AL","AL   "\n\t" \
                  "jge "LR(1,f)     "\n\t" \
                  "andb $127,"AL    "\n\t" \
                  "sall $8,"EAX     "\n\t" \
                  "movb (%1),"AL    "\n\t" \
                  "incl %1"         "\n"   \
                  LD(1)":"                 \
                  : OUT_EAX (where), "=r" (byteptr) : "1" (byteptr) );
              # Vorsicht: 1. Der Sun-Assembler kennt diese Syntax für lokale Labels nicht.
              #              Daher generieren wir unsere lokalen Labels selbst.
              # Vorsicht: 2. ccr wird verändert. Wie deklariert man das??
            #endif
            #if defined(GNU) && defined(ARM) && !defined(NO_ASM)
              # Macros written by Peter Burwood.
              # Two versions. Which one to choose?
              #        instructions      short case        long case
              # v1:          5           2 + 3 skipped     5
              # v2:          5           3 + 2 skipped     4 + 1 skipped
              # Let's choose the first one. 1-byte operands are most frequent.
              #undef U_operand
              #define U_operand(where)  # (v1) \
                { var uintL dummy;                 \
                  __asm__(                         \
                    "ldrb   %0,[%1],#1"     "\n\t" \
                    "tst    %0,#0x80"       "\n\t" \
                    "bicne  %0,%0,#0x80"    "\n\t" \
                    "ldrneb %2,[%1],#1"     "\n\t" \
                    "orrne  %0,%2,%0,LSL#8"        \
                    : "=r" (where), "=r" (byteptr), "=r" (dummy) : "1" (byteptr) : "cc" ); \
                }
              #if 0
              #undef U_operand
              #define U_operand(where)  # (v2) \
                { var uintL dummy;                 \
                  __asm__(                         \
                    "ldrb   %0,[%1],#1"     "\n\t" \
                    "movs   %0,%0,LSL#25"   "\n\t" \
                    "movcc  %0,%0,LSR#25"   "\n\t" \
                    "ldrcsb %2,[%1],#1"     "\n\t" \
                    "orrcs  %0,%2,%0,LSR#17"       \
                    : "=r" (where), "=r" (byteptr), "=r" (dummy) : "1" (byteptr) : "cc" ); \
                }
              #endif
            #endif
            #
            # Macro S_operand(where);
            # bringt den nächsten Operanden (ein Signed Integer)
            # nach (uintL)where und rückt dabei den Bytecodepointer weiter.
              #define S_operand(where)  \
                { where = *byteptr++; # erstes Byte lesen              \
                  if ((uintB)where & bit(7))                           \
                    # Bit 7 war gesetzt                                \
                    { where = where << 8;                              \
                      where |= *byteptr++; # nächstes Byte dazunehmen  \
                      # Sign-Extend von 15 auf 32 Bits:                \
                      where = (sintL)((sintL)(sintWL)((sintWL)where << (intWLsize-15)) >> (intWLsize-15)); \
                      if (where == 0)                                  \
                        # Sonderfall: 2-Byte-Operand = 0 -> 6-Byte-Operand \
                        { where = (uintL)( ((uintWL)(byteptr[0]) << 8) \
                                          | (uintWL)(byteptr[1])       \
                                         ) << 16                       \
                                | (uintL)( ((uintWL)(byteptr[2]) << 8) \
                                          | (uintWL)(byteptr[3])       \
                                         );                            \
                          byteptr += 4;                                \
                    }   }                                              \
                    else                                               \
                    # Bit 7 war gelöscht                               \
                    { # Sign-Extend von 7 auf 32 Bits:                 \
                      where = (sintL)((sintL)(sintBWL)((sintBWL)where << (intBWLsize-7)) >> (intBWLsize-7)); \
                    }                                                  \
                }
            #if defined(GNU) && defined(MC680X0) && !defined(NO_ASM)
              #undef S_operand
              #define S_operand(where)  \
                __asm__(                   \
                  "moveb %1@+,%0"   "\n\t" \
                  "bpl 1f"          "\n\t" \
                  "lslw #8,%0"      "\n\t" \
                  "moveb %1@+,%0"   "\n\t" \
                  "addw %0,%0"      "\n\t" \
                  "asrw #1,%0"      "\n\t" \
                  "bne 2f"          "\n\t" \
                  "moveb %1@(2),%0" "\n\t" \
                  "swap %0"         "\n\t" \
                  "moveb %1@+,%0"   "\n\t" \
                  "lsll #8,%0"      "\n\t" \
                  "moveb %1@,%0"    "\n\t" \
                  "swap %0"         "\n\t" \
                  "addql #2,%0"     "\n\t" \
                  "moveb %1@+,%0"   "\n\t" \
                  "jra 3f"          "\n"   \
                  "1:"                "\t" \
                  "addb %0,%0"      "\n\t" \
                  "asrb #1,%0"      "\n\t" \
                  "extw %0"         "\n"   \
                  "2:"                "\t" \
                  "extl %0"         "\n"   \
                  "3:"                     \
                  : "=d" (where), "=a" (byteptr) : "1" (byteptr) )
            #endif
            #if defined(GNU) && defined(SPARC) && !defined(NO_ASM)
              #undef S_operand
              #define S_operand(where)  \
                { var uintL dummy;              \
                  __asm__(                      \
                    "ldub [%1],%0"       "\n\t" \
                    "andcc %0,0x80,%%g0" "\n\t" \
                    "be 2f"              "\n\t" \
                    " add %1,1,%1"       "\n\t" \
                    "sll %0,25,%2"       "\n\t" \
                    "ldub [%1],%0"       "\n\t" \
                    "sra %2,17,%2"       "\n\t" \
                    "orcc %2,%0,%0"      "\n\t" \
                    "bne 3f"             "\n\t" \
                    " add %1,1,%1"       "\n\t" \
                    "ldub [%1],%0"       "\n\t" \
                    "sll %0,24,%2"       "\n\t" \
                    "ldub [%1+1],%0"     "\n\t" \
                    "sll %0,16,%0"       "\n\t" \
                    "or %2,%0,%2"        "\n\t" \
                    "ldub [%1+2],%0"     "\n\t" \
                    "sll %0,8,%0"        "\n\t" \
                    "or %2,%0,%2"        "\n\t" \
                    "ldub [%1+3],%0"     "\n\t" \
                    "or %2,%0,%0"        "\n\t" \
                    "b 3f"               "\n\t" \
                    " add %1,4,%1"       "\n"   \
                    "2:"                   "\t" \
                    "sll %0,25,%0"       "\n\t" \
                    "sra %0,25,%0"       "\n"   \
                    "3:"                   "\t" \
                    : "=r" (where), "=r" (byteptr), "=r" (dummy) : "1" (byteptr) : "ccr" ); \
                }
            #endif
            #if defined(GNU) && defined(I80386) && !defined(NO_ASM)
              #undef S_operand
              #define S_operand(where)  \
                __asm__(                   \
                  "movzbl (%1),"EAX "\n\t" \
                  "incl %1"         "\n\t" \
                  "testb "AL","AL   "\n\t" \
                  "jge "LR(1,f)     "\n\t" \
                  "sall $8,"EAX     "\n\t" \
                  "movb (%1),"AL    "\n\t" \
                  "incl %1"         "\n\t" \
                  "sall $17,"EAX    "\n\t" \
                  "sarl $17,"EAX    "\n\t" \
                  "jne "LR(2,f)     "\n\t" \
                  "movb (%1),"AL    "\n\t" \
                  "sall $8,"EAX     "\n\t" \
                  "movb 1(%1),"AL   "\n\t" \
                  "sall $8,"EAX     "\n\t" \
                  "movb 2(%1),"AL   "\n\t" \
                  "sall $8,"EAX     "\n\t" \
                  "movb 3(%1),"AL   "\n\t" \
                  "addl $4,"EAX     "\n\t" \
                  "jmp "LR(2,f)     "\n"   \
                  LD(1)":"            "\t" \
                  "sall $25,"EAX    "\n\t" \
                  "sarl $25,"EAX    "\n"   \
                  LD(2)":"                 \
                  : OUT_EAX (where), "=r" (byteptr) : "1" (byteptr) );
            #endif
            #if defined(GNU) && defined(ARM) && !defined(NO_ASM)
              # Macro written by Peter Burwood.
              #undef S_operand
              #define S_operand(where)  \
                { var uintL dummy;                  \
                  __asm__(                          \
                    "ldrb   %0,[%1],#1"      "\n\t" \
                    "movs   %0,%0,LSL#25"    "\n\t" \
                    "movcc  %0,%0,ASR#25"    "\n\t" \
                    "bcc    "LR(1,f)         "\n\t" \
                    "ldrb   %2,[%1],#1"      "\n\t" \
                    "orr    %0,%0,%2,LSL#17" "\n\t" \
                    "movs   %0,%0,ASR#17"    "\n\t" \
                    "bne    "LR(1,f)         "\n\t" \
                    "ldrb   %0,[%1],#1"      "\n\t" \
                    "ldrb   %2,[%1],#1"      "\n\t" \
                    "orr    %0,%2,%0,LSL#8"  "\n\t" \
                    "ldrb   %2,[%1],#1"      "\n\t" \
                    "orr    %0,%2,%0,LSL#8"  "\n\t" \
                    "ldrb   %2,[%1],#1"      "\n\t" \
                    "orr    %0,%2,%0,LSL#8"  "\n"   \
                    LD(1)":"                        \
                    : "=r" (where), "=r" (byteptr), "=r" (dummy) : "1" (byteptr) : "cc" ); \
                }
            #endif
            #
            # Macro S_operand_ignore();
            # übergeht den nächsten Operanden (ein Signed Integer)
            # und rückt dabei den Bytecodepointer weiter.
              #define S_operand_ignore()  \
                { var uintB where = *byteptr++; # erstes Byte lesen        \
                  if ((uintB)where & bit(7))                               \
                    # Bit 7 war gesetzt                                    \
                    { if ((uintB)((where<<1) | *byteptr++) == 0) # nächstes Byte dazu \
                        # Sonderfall: 2-Byte-Operand = 0 -> 6-Byte-Operand \
                        { byteptr += 4; }                                  \
                }   }
            #if defined(GNU) && defined(MC680X0) && !defined(NO_ASM)
              #undef S_operand_ignore
              #define S_operand_ignore()  \
                { var uintB where;           \
                  __asm__(                   \
                    "moveb %1@+,%0"   "\n\t" \
                    "bpl 1f"          "\n\t" \
                    "addb %0,%0"      "\n\t" \
                    "orb %1@+,%0"     "\n\t" \
                    "bne 1f"          "\n\t" \
                    "addql #4,%1"     "\n"   \
                    "1:"                     \
                    "=d" (where), "=a" (byteptr) : "1" (byteptr) ); \
                }
            #endif
            #if defined(GNU) && defined(SPARC) && !defined(NO_ASM)
              #undef S_operand_ignore
              #define S_operand_ignore()  \
                { var uintL where;              \
                  var uintL dummy;              \
                  __asm__(                      \
                    "ldub [%1],%0"       "\n\t" \
                    "andcc %0,0x80,%%g0" "\n\t" \
                    "be 1f"              "\n\t" \
                    " add %1,1,%1"       "\n\t" \
                    "sll %0,1,%2"        "\n\t" \
                    "ldub [%1],%0"       "\n\t" \
                    "orcc %2,%0,%0"      "\n\t" \
                    "bne 1f"             "\n\t" \
                    " add %1,1,%1"       "\n\t" \
                    "add %1,4,%1"        "\n"   \
                    "1:"                        \
                    : "=r" (where), "=r" (byteptr), "=r" (dummy) : "1" (byteptr) : "ccr" ); \
                }
            #endif
            #if defined(GNU) && defined(ARM) && !defined(NO_ASM)
              # Macro written by Peter Burwood.
              #undef S_operand_ignore
              #define S_operand_ignore()  \
                { var uintL where;                  \
                  var uintL dummy;                  \
                  __asm__(                          \
                    "ldrb   %0,[%1],#1"      "\n\t" \
                    "movs   %0,%0,LSL#25"    "\n\t" \
                    "bcc    "LR(1,f)         "\n\t" \
                    "ldrb   %2,[%1],#1"      "\n\t" \
                    "orrs   %0,%2,%0,LSR#24" "\n\t" \
                    "addeq  %1,%1,#4"        "\n"   \
                    LD(1)":"                        \
                    : "=r" (where), "=r" (byteptr), "=r" (dummy) : "1" (byteptr) : "cc" ); \
                }
            #endif
            #
            # Macro L_operand(where);
            # bringt den nächsten Operanden (ein Label)
            # nach (uintB*)where und rückt dabei den Bytecodepointer weiter.
              #define L_operand(Lwhere)  \
                { var uintL where; # Variable fürs Displacement \
                  S_operand(where); # Displacement              \
                  Lwhere = byteptr + (sintL)where; # addieren   \
                }
            #
            # Macro L_operand_ignore();
            # übergeht den nächsten Operanden (ein Label)
            # und rückt dabei den Bytecodepointer weiter.
              #define L_operand_ignore()  S_operand_ignore()
            #
            # Die einzelnen Bytecodes werden interpretiert:
            # Dabei ist meist mv_count/mv_space = Werte,
            # closureptr = Pointer auf die compilierte Closure im Stack,
            # closure = compilierte Closure,
            # codeptr = Pointer auf ihren Codevektor,
            # byteptr = Pointer auf das nächste Byte im Code.
            # (byteptr ist kein LISP-Objekt, aber dennoch GC-gefährdet! Um es
            #  GC-invariant zu machen, muss man CODEPTR
            #  davon subtrahieren. Addiert man dann Fixnum_0 dazu,
            #  so hat man die Bytenummer als Fixnum.)
            #if 0
              #define CODEPTR  (&codeptr->data[0])
            #else # liefert effizienteren Code
              #define CODEPTR  (uintB*)(codeptr)
            #endif
            #
            # Kontextinformation aufbewahren:
            # Wird etwas aufgerufen, das eine GC auslösen kann, so muss dies in ein
            # with_saved_context( ... ) eingebaut werden.
              #define with_saved_context(statement)  \
                { var uintL index = byteptr - CODEPTR;                       \
                  statement;                                                 \
                  closure = *closureptr; # Closure aus dem Stack holen       \
                  codeptr = TheSbvector(TheCclosure(closure)->clos_codevec); \
                  byteptr = CODEPTR + index;                                 \
                }
            #
            # ------------------- (1) Konstanten -----------------------
            CASE cod_nil:                    # (NIL)
              code_nil:
              value1 = NIL; mv_count = 1;
              goto next_byte;
            CASE cod_nil_push:               # (NIL&PUSH)
              pushSTACK(NIL);
              goto next_byte;
            CASE cod_push_nil:               # (PUSH-NIL n)
              { var uintC n;
                U_operand(n);
                dotimesC(n,n, { pushSTACK(NIL); } );
              }
              goto next_byte;
            CASE cod_t:                      # (T)
              code_t:
              value1 = T; mv_count = 1;
              goto next_byte;
            CASE cod_t_push:                 # (T&PUSH)
              pushSTACK(T);
              goto next_byte;
            CASE cod_const:                  # (CONST n)
              { var uintL n;
                U_operand(n);
                value1 = TheCclosure(closure)->clos_consts[n]; mv_count=1;
              }
              goto next_byte;
            CASE cod_const_push:             # (CONST&PUSH n)
              { var uintL n;
                U_operand(n);
                pushSTACK(TheCclosure(closure)->clos_consts[n]);
              }
              goto next_byte;
            # ------------------- (2) statische Variablen -----------------------
            CASE cod_load:                   # (LOAD n)
              { var uintL n;
                U_operand(n);
                value1 = STACK_(n); mv_count=1;
              }
              goto next_byte;
            CASE cod_load_push:              # (LOAD&PUSH n)
              { var uintL n;
                U_operand(n);
                pushSTACK(STACK_(n));
              }
              goto next_byte;
            CASE cod_loadi:                  # (LOADI k1 k2 n)
              { var uintL k1;
                var uintL k2;
                var uintL n;
                U_operand(k1);
                U_operand(k2);
                U_operand(n);
               {var object* FRAME = (object*) SP_(k1+jmpbufsize*k2);
                value1 = FRAME_(n); mv_count=1;
              }}
              goto next_byte;
            CASE cod_loadi_push:             # (LOADI&PUSH k1 k2 n)
              { var uintL k1;
                var uintL k2;
                var uintL n;
                U_operand(k1);
                U_operand(k2);
                U_operand(n);
               {var object* FRAME = (object*) SP_(k1+jmpbufsize*k2);
                pushSTACK(FRAME_(n));
              }}
              goto next_byte;
            CASE cod_loadc:                  # (LOADC n m)
              { var uintL n;
                var uintL m;
                U_operand(n);
                U_operand(m);
                value1 = TheSvector(STACK_(n))->data[1+m]; mv_count=1;
              }
              goto next_byte;
            CASE cod_loadc_push:             # (LOADC&PUSH n m)
              { var uintL n;
                var uintL m;
                U_operand(n);
                U_operand(m);
                pushSTACK(TheSvector(STACK_(n))->data[1+m]);
              }
              goto next_byte;
            CASE cod_loadv:                  # (LOADV k m)
              { var uintC k;
                var uintL m;
                U_operand(k);
                U_operand(m);
               {var object venv = TheCclosure(closure)->clos_venv; # VenvConst
                # k mal (svref ... 0) nehmen:
                dotimesC(k,k, { venv = TheSvector(venv)->data[0]; } );
                # (svref ... m) holen:
                value1 = TheSvector(venv)->data[m]; mv_count=1;
              }}
              goto next_byte;
            CASE cod_loadv_push:             # (LOADV&PUSH k m)
              { var uintC k;
                var uintL m;
                U_operand(k);
                U_operand(m);
               {var object venv = TheCclosure(closure)->clos_venv; # VenvConst
                # k mal (svref ... 0) nehmen:
                dotimesC(k,k, { venv = TheSvector(venv)->data[0]; } );
                # (svref ... m) holen:
                pushSTACK(TheSvector(venv)->data[m]);
              }}
              goto next_byte;
            CASE cod_loadic:                 # (LOADIC k1 k2 n m)
              { var uintL k1;
                var uintL k2;
                var uintL n;
                var uintL m;
                U_operand(k1);
                U_operand(k2);
                U_operand(n);
                U_operand(m);
               {var object* FRAME = (object*) SP_(k1+jmpbufsize*k2);
                value1 = TheSvector(FRAME_(n))->data[1+m]; mv_count=1;
              }}
              goto next_byte;
            CASE cod_store: store:           # (STORE n)
              { var uintL n;
                U_operand(n);
                STACK_(n) = value1; mv_count=1;
              }
              goto next_byte;
            CASE cod_pop_store:              # (POP&STORE n)
              { var uintL n;
                U_operand(n);
               {var object obj = popSTACK();
                STACK_(n) = value1 = obj; mv_count=1;
              }}
              goto next_byte;
            CASE cod_storei:                 # (STOREI k1 k2 n)
              { var uintL k1;
                var uintL k2;
                var uintL n;
                U_operand(k1);
                U_operand(k2);
                U_operand(n);
               {var object* FRAME = (object*) SP_(k1+jmpbufsize*k2);
                FRAME_(n) = value1; mv_count=1;
              }}
              goto next_byte;
            CASE cod_load_storec:            # (LOAD&STOREC k m n)
              { var uintL k;
                U_operand(k);
                value1 = STACK_(k);
              }
            CASE cod_storec:                 # (STOREC n m)
              { var uintL n;
                var uintL m;
                U_operand(n);
                U_operand(m);
                TheSvector(STACK_(n))->data[1+m] = value1; mv_count=1;
              }
              goto next_byte;
            CASE cod_storev:                 # (STOREV k m)
              { var uintC k;
                var uintL m;
                U_operand(k);
                U_operand(m);
               {var object venv = TheCclosure(closure)->clos_venv; # VenvConst
                # k mal (svref ... 0) nehmen:
                dotimesC(k,k, { venv = TheSvector(venv)->data[0]; } );
                # (svref ... m) abspeichern:
                TheSvector(venv)->data[m] = value1; mv_count=1;
              }}
              goto next_byte;
            CASE cod_storeic:                # (STOREIC k1 k2 n m)
              { var uintL k1;
                var uintL k2;
                var uintL n;
                var uintL m;
                U_operand(k1);
                U_operand(k2);
                U_operand(n);
                U_operand(m);
               {var object* FRAME = (object*) SP_(k1+jmpbufsize*k2);
                TheSvector(FRAME_(n))->data[1+m] = value1; mv_count=1;
              }}
              goto next_byte;
            # ------------------- (3) dynamische Variablen -----------------------
            CASE cod_getvalue:               # (GETVALUE n)
              { var uintL n;
                U_operand(n);
               {var object symbol = TheCclosure(closure)->clos_consts[n];
                # Der Compiler hat schon überprüft, dass es ein Symbol ist.
                if (eq(Symbol_value(symbol),unbound))
                  { pushSTACK(symbol); # Wert für Slot NAME von CELL-ERROR
                    pushSTACK(symbol);
                    fehler(unbound_variable,
                           GETTEXT("symbol ~ has no value")
                          );
                  }
                value1 = Symbol_value(symbol); mv_count=1;
              }}
              goto next_byte;
            CASE cod_getvalue_push:          # (GETVALUE&PUSH n)
              { var uintL n;
                U_operand(n);
               {var object symbol = TheCclosure(closure)->clos_consts[n];
                # Der Compiler hat schon überprüft, dass es ein Symbol ist.
                if (eq(Symbol_value(symbol),unbound))
                  { pushSTACK(symbol); # Wert für Slot NAME von CELL-ERROR
                    pushSTACK(symbol);
                    fehler(unbound_variable,
                           GETTEXT("symbol ~ has no value")
                          );
                  }
                pushSTACK(Symbol_value(symbol));
              }}
              goto next_byte;
            CASE cod_setvalue:               # (SETVALUE n)
              { var uintL n;
                U_operand(n);
               {var object symbol = TheCclosure(closure)->clos_consts[n];
                # Der Compiler hat schon überprüft, dass es ein Symbol ist.
                if (constantp(TheSymbol(symbol)))
                  { pushSTACK(symbol);
                    fehler(error,
                           GETTEXT("assignment to constant symbol ~ is impossible")
                          );
                  }
                Symbol_value(symbol) = value1; mv_count=1;
              }}
              goto next_byte;
            CASE cod_bind:                   # (BIND n)
              { var uintL n;
                U_operand(n);
                dynamic_bind(TheCclosure(closure)->clos_consts[n],value1);
              }
              goto next_byte;
            CASE cod_unbind1:                # (UNBIND1)
              #if STACKCHECKC
              if (!(framecode(STACK_0) == DYNBIND_frame_info))
                goto fehler_STACK_putt;
              #endif
              # Variablenbindungsframe auflösen:
              { var object* new_STACK = topofframe(STACK_0); # Pointer übern Frame
                var object* frame_end = STACKpointable(new_STACK);
                var object* bindingptr = &STACK_1; # Beginn der Bindungen
                # bindingptr läuft durch die Bindungen hoch
                until (bindingptr == frame_end)
                  { # alten Wert zurückschreiben:
                    Symbol_value(*(bindingptr STACKop 0)) = *(bindingptr STACKop 1);
                    bindingptr skipSTACKop 2; # nächste Bindung
                  }
                # STACK neu setzen, dadurch Frame auflösen:
                setSTACK(STACK = new_STACK);
              }
              goto next_byte;
            CASE cod_unbind:                 # (UNBIND n)
              { var uintC n;
                U_operand(n); # n>0
               {var object* FRAME = STACK;
                do {
                    #if STACKCHECKC
                    if (!(framecode(FRAME_(0)) == DYNBIND_frame_info))
                      goto fehler_STACK_putt;
                    #endif
                    # Variablenbindungsframe auflösen:
                    { var object* new_FRAME = topofframe(FRAME_(0)); # Pointer übern Frame
                      var object* frame_end = STACKpointable(new_FRAME);
                      var object* bindingptr = &FRAME_(1); # Beginn der Bindungen
                      # bindingptr läuft durch die Bindungen hoch
                      until (bindingptr == frame_end)
                        { # alten Wert zurückschreiben:
                          Symbol_value(*(bindingptr STACKop 0)) = *(bindingptr STACKop 1);
                          bindingptr skipSTACKop 2; # nächste Bindung
                        }
                      FRAME = new_FRAME;
                   }}
                   until (--n == 0);
                setSTACK(STACK = FRAME); # STACK neu setzen
              }}
              goto next_byte;
            CASE cod_progv:                  # (PROGV)
              { var object vallist = value1; # Wertliste
                var object symlist = popSTACK(); # Symbolliste
                pushSP((aint)STACK); # STACK in den SP legen
                progv(symlist,vallist); # Frame aufbauen
              }
              goto next_byte;
            # ------------------- (4) Stackoperationen -----------------------
            CASE cod_push:                   # (PUSH)
              pushSTACK(value1);
              goto next_byte;
            CASE cod_pop:                    # (POP)
              value1 = popSTACK(); mv_count=1;
              goto next_byte;
            CASE cod_skip:                   # (SKIP n)
              { var uintL n;
                U_operand(n);
                skipSTACK(n);
              }
              goto next_byte;
            CASE cod_skipi:                  # (SKIPI k1 k2 n)
              { var uintL k1;
                var uintL k2;
                var uintL n;
                U_operand(k1);
                U_operand(k2);
                U_operand(n);
                skipSP(k1+jmpbufsize*k2);
               {var object* newSTACK;
                popSP( newSTACK = (object*) );
                setSTACK(STACK = newSTACK STACKop n);
              }}
              goto next_byte;
            CASE cod_skipsp:                 # (SKIPSP k1 k2)
              { var uintL k1;
                var uintL k2;
                U_operand(k1);
                U_operand(k2);
                skipSP(k1+jmpbufsize*k2);
              }
              goto next_byte;
            # ------------------- (5) Programmfluss und Sprünge -----------------------
            CASE cod_skip_ret:               # (SKIP&RET n)
              { var uintL n;
                U_operand(n);
                skipSTACK(n);
                goto finished; # Rücksprung zum Aufrufer
              }
            CASE cod_skip_retgf:             # (SKIP&RETGF n)
              { var uintL n;
                U_operand(n);
                if (((Codevec)codeptr)->ccv_flags & bit(3)) # Aufrufhemmung?
                  { skipSTACK(n);
                    mv_count=1;
                    goto finished; # Rücksprung zum Aufrufer
                  }
                # Es ist bekannt (siehe clos.lsp), dass diese Funktion
                # keine optionalen Parameter hat, aber evtl. Rest-Parameter.
                # Falls kein Rest-Parameter: (FUNCALL value1 arg1 ... argr)
                # Falls mit Rest-Parameter: (APPLY value1 arg1 ... argr restarg)
               {var uintL r = ((Codevec)codeptr)->ccv_numreq;
                n -= r;
                if (((Codevec)codeptr)->ccv_flags & bit(0))
                  { skipSTACK(n-1); apply(value1,r,popSTACK()); }
                else
                  { skipSTACK(n); funcall(value1,r); }
                goto finished; # Rücksprung zum Aufrufer
              }}
            #define JMP()  \
              { var const uintB* label_byteptr; \
                L_operand(label_byteptr);       \
                byteptr = label_byteptr;        \
                goto next_byte;                 \
              }
            #define NOTJMP()  \
              { L_operand_ignore(); goto next_byte; }
            jmp1: mv_count=1;
            CASE cod_jmp: jmp:               # (JMP label)
              JMP();
            CASE cod_jmpif:                  # (JMPIF label)
              if (!nullp(value1)) goto jmp;
              notjmp:
              NOTJMP();
            CASE cod_jmpifnot:               # (JMPIFNOT label)
              if (nullp(value1)) goto jmp;
              NOTJMP();
            CASE cod_jmpif1:                 # (JMPIF1 label)
              if (!nullp(value1)) goto jmp1;
              NOTJMP();
            CASE cod_jmpifnot1:              # (JMPIFNOT1 label)
              if (nullp(value1)) goto jmp1;
              NOTJMP();
            CASE cod_jmpifatom:              # (JMPIFATOM label)
              if (atomp(value1)) goto jmp;
              NOTJMP();
            CASE cod_jmpifconsp:             # (JMPIFCONSP label)
              if (consp(value1)) goto jmp;
              NOTJMP();
            CASE cod_jmpifeq:                # (JMPIFEQ label)
              if (eq(popSTACK(),value1)) goto jmp;
              NOTJMP();
            CASE cod_jmpifnoteq:             # (JMPIFNOTEQ label)
              if (!eq(popSTACK(),value1)) goto jmp;
              NOTJMP();
            CASE cod_jmpifeqto:              # (JMPIFEQTO n label)
              { var uintL n;
                U_operand(n);
                if (eq(popSTACK(),TheCclosure(closure)->clos_consts[n])) goto jmp;
              }
              NOTJMP();
            CASE cod_jmpifnoteqto:           # (JMPIFNOTEQTO n label)
              { var uintL n;
                U_operand(n);
                if (!eq(popSTACK(),TheCclosure(closure)->clos_consts[n])) goto jmp;
              }
              NOTJMP();
            CASE cod_jmphash:                # (JMPHASH n label)
              { var uintL n;
                U_operand(n);
               {var object hashvalue = # value1 in der Hash-Tabelle suchen
                  gethash(value1,TheCclosure(closure)->clos_consts[n]);
                if (eq(hashvalue,nullobj))
                  goto jmp; # nicht gefunden -> zu label springen
                  else # gefundenes Fixnum als Label interpretieren:
                  { byteptr += fixnum_to_L(hashvalue); }
              }}
              goto next_byte;
            CASE cod_jmphashv:               # (JMPHASHV n label)
              { var uintL n;
                U_operand(n);
               {var object hashvalue = # value1 in der Hash-Tabelle suchen
                  gethash(value1,TheSvector(TheCclosure(closure)->clos_consts[0])->data[n]);
                if (eq(hashvalue,nullobj))
                  goto jmp; # nicht gefunden -> zu label springen
                  else # gefundenes Fixnum als Label interpretieren:
                  { byteptr += fixnum_to_L(hashvalue); }
              }}
              goto next_byte;
            # Führt einen (JSR label)-Befehl aus.
            #define JSR()  \
              check_STACK(); check_SP();                              \
              { var const uintB* label_byteptr;                       \
                L_operand(label_byteptr);                             \
                with_saved_context(                                   \
                  interpret_bytecode_(closure,codeptr,label_byteptr); \
                  );                                                  \
              }
            CASE cod_jsr:                    # (JSR label)
              JSR();
              goto next_byte;
            CASE cod_jsr_push:               # (JSR&PUSH label)
              JSR(); pushSTACK(value1);
              goto next_byte;
            CASE cod_jmptail:                # (JMPTAIL m n label)
              { var uintL m;
                var uintL n;
                U_operand(m);
                U_operand(n);
                # Es gilt n>=m. m Stackeinträge um n-m nach oben kopieren:
               {var object* ptr1 = STACK STACKop m;
                var object* ptr2 = STACK STACKop n;
                var uintC count;
                dotimesC(count,m, { NEXT(ptr2) = NEXT(ptr1); } );
                # Nun ist ptr1 = STACK und ptr2 = STACK STACKop (n-m).
                *(closureptr = &NEXT(ptr2)) = closure; # Closure im Stack ablegen
                setSTACK(STACK = ptr2); # STACK verkürzen
              }}
              JMP(); # an label springen
            # ------------------- (6) Environments und Closures -----------------------
            CASE cod_venv:                   # (VENV)
              # VenvConst aus der Closure holen:
              value1 = TheCclosure(closure)->clos_venv; mv_count=1;
              goto next_byte;
            CASE cod_make_vector1_push:      # (MAKE-VECTOR1&PUSH n)
              { var uintL n;
                U_operand(n);
                pushSTACK(value1);
                # Vektor erzeugen:
               {var object vec;
                with_saved_context( { vec = allocate_vector(n+1); } );
                # Erstes Element eintragen:
                TheSvector(vec)->data[0] = STACK_0;
                STACK_0 = vec;
              }}
              goto next_byte;
            CASE cod_copy_closure:           # (COPY-CLOSURE m n)
              { var object oldclos;
                # zu kopierende Closure holen:
               {var uintL m;
                U_operand(m);
                oldclos = TheCclosure(closure)->clos_consts[m];
               }
                # Closure gleicher Länge allozieren:
               {var object newclos;
                pushSTACK(oldclos);
                with_saved_context(
                  newclos = allocate_cclosure_copy(oldclos);
                  );
                oldclos = popSTACK();
                # Inhalt der alten in die neue Closure kopieren:
                do_cclosure_copy(newclos,oldclos);
                # Stackinhalt in die neue Closure kopieren:
                { var uintL n;
                  U_operand(n);
                 {var object* newptr = &TheCclosure(newclos)->clos_consts[n];
                  dotimespL(n,n, { *--newptr = popSTACK(); } );
                }}
                value1 = newclos; mv_count=1;
              }}
              goto next_byte;
            CASE cod_copy_closure_push:      # (COPY-CLOSURE&PUSH m n)
              { var object oldclos;
                # zu kopierende Closure holen:
               {var uintL m;
                U_operand(m);
                oldclos = TheCclosure(closure)->clos_consts[m];
               }
                # Closure gleicher Länge allozieren:
               {var object newclos;
                pushSTACK(oldclos);
                with_saved_context(
                  newclos = allocate_cclosure_copy(oldclos);
                  );
                oldclos = popSTACK();
                # Inhalt der alten in die neue Closure kopieren:
                do_cclosure_copy(newclos,oldclos);
                # Stackinhalt in die neue Closure kopieren:
                { var uintL n;
                  U_operand(n);
                 {var object* newptr = &TheCclosure(newclos)->clos_consts[n];
                  dotimespL(n,n, { *--newptr = popSTACK(); } );
                }}
                pushSTACK(newclos);
              }}
              goto next_byte;
            # ------------------- (7) Funktionsaufrufe -----------------------
            # Führt (CALL k n)-Befehl aus.
            #define CALL()  \
              { var uintC k; # Argumentezahl                       \
                var uintL n;                                       \
                U_operand(k);                                      \
                U_operand(n);                                      \
                with_saved_context(                                \
                  funcall(TheCclosure(closure)->clos_consts[n],k); \
                  );                                               \
              }
            # Führt (CALL0 n)-Befehl aus.
            #define CALL0()  \
              { var uintL n;                                       \
                U_operand(n);                                      \
                with_saved_context(                                \
                  funcall(TheCclosure(closure)->clos_consts[n],0); \
                  );                                               \
              }
            # Führt (CALL1 n)-Befehl aus.
            #define CALL1()  \
              { var uintL n;                                       \
                U_operand(n);                                      \
                with_saved_context(                                \
                  funcall(TheCclosure(closure)->clos_consts[n],1); \
                  );                                               \
              }
            # Führt (CALL2 n)-Befehl aus.
            #define CALL2()  \
              { var uintL n;                                       \
                U_operand(n);                                      \
                with_saved_context(                                \
                  funcall(TheCclosure(closure)->clos_consts[n],2); \
                  );                                               \
              }
            # Führt (CALLS1 n)-Befehl aus.
            #define CALLS1()  \
              { var uintL n;                                              \
                B_operand(n);                                             \
                # Der Compiler hat die Argumentüberprüfung schon gemacht. \
               {var Subr fun = FUNTAB1[n];                                \
                subr_self = subr_tab_ptr_as_object(fun);                  \
                with_saved_context(                                       \
                  (*(subr_norest_function*)(fun->function))();            \
                  );                                                      \
              }}
            # Führt (CALLS2 n)-Befehl aus.
            #define CALLS2()  \
              { var uintL n;                                              \
                B_operand(n);                                             \
                # Der Compiler hat die Argumentüberprüfung schon gemacht. \
               {var Subr fun = FUNTAB2[n];                                \
                subr_self = subr_tab_ptr_as_object(fun);                  \
                with_saved_context(                                       \
                  (*(subr_norest_function*)(fun->function))();            \
                  );                                                      \
              }}
            # Führt (CALLSR m n)-Befehl aus.
            #define CALLSR()  \
              { var uintL m;                                              \
                var uintL n;                                              \
                U_operand(m);                                             \
                B_operand(n);                                             \
                # Der Compiler hat die Argumentüberprüfung schon gemacht. \
               {var Subr fun = FUNTABR[n];                                \
                subr_self = subr_tab_ptr_as_object(fun);                  \
                with_saved_context(                                       \
                  (*(subr_rest_function*)(fun->function))(m,args_end_pointer STACKop m); \
                  );                                                      \
              }}
            CASE cod_call:                   # (CALL k n)
              CALL();
              goto next_byte;
            CASE cod_call_push:              # (CALL&PUSH k n)
              CALL(); pushSTACK(value1);
              goto next_byte;
            CASE cod_call0:                  # (CALL0 n)
              CALL0();
              goto next_byte;
            CASE cod_call1:                  # (CALL1 n)
              CALL1();
              goto next_byte;
            CASE cod_call1_push:             # (CALL1&PUSH n)
              CALL1(); pushSTACK(value1);
              goto next_byte;
            CASE cod_call2:                  # (CALL2 n)
              CALL2();
              goto next_byte;
            CASE cod_call2_push:             # (CALL2&PUSH n)
              CALL2(); pushSTACK(value1);
              goto next_byte;
            CASE cod_calls1:                 # (CALLS1 n)
              CALLS1();
              goto next_byte;
            CASE cod_calls1_push:            # (CALLS1&PUSH n)
              CALLS1(); pushSTACK(value1);
              goto next_byte;
            CASE cod_calls2:                 # (CALLS2 n)
              CALLS2();
              goto next_byte;
            CASE cod_calls2_push:            # (CALLS2&PUSH n)
              CALLS2(); pushSTACK(value1);
              goto next_byte;
            CASE cod_callsr:                 # (CALLSR m n)
              CALLSR();
              goto next_byte;
            CASE cod_callsr_push:            # (CALLSR&PUSH m n)
              CALLSR(); pushSTACK(value1);
              goto next_byte;
            # Führt einen (CALLC)-Befehl aus.
            #define CALLC()  \
              { check_STACK(); check_SP(); # STACK und SP überprüfen \
                with_saved_context(                                  \
                  # compilierte Closure ab Byte 8 interpretieren:    \
                  interpret_bytecode(value1,TheCclosure(value1)->clos_codevec,CCV_START_NONKEY); \
                  );                                                 \
              }
            # Führt einen (CALLCKEY)-Befehl aus.
            #define CALLCKEY()  \
              { check_STACK(); check_SP(); # STACK und SP überprüfen \
                with_saved_context(                                  \
                  # compilierte Closure ab Byte 12 interpretieren:   \
                  interpret_bytecode(value1,TheCclosure(value1)->clos_codevec,CCV_START_KEY); \
                  );                                                 \
              }
            CASE cod_callc:                  # (CALLC)
              CALLC();
              goto next_byte;
            CASE cod_callc_push:             # (CALLC&PUSH)
              CALLC(); pushSTACK(value1);
              goto next_byte;
            CASE cod_callckey:               # (CALLCKEY)
              CALLCKEY();
              goto next_byte;
            CASE cod_callckey_push:          # (CALLCKEY&PUSH)
              CALLCKEY(); pushSTACK(value1);
              goto next_byte;
            CASE cod_funcall:                # (FUNCALL n)
              { var uintL n;
                U_operand(n);
               {var object fun = STACK_(n); # Funktion
                with_saved_context( funcall(fun,n); ); # Funktion aufrufen
                skipSTACK(1); # Funktion aus dem Stack streichen
              }}
              goto next_byte;
            CASE cod_funcall_push:           # (FUNCALL&PUSH n)
              { var uintL n;
                U_operand(n);
               {var object fun = STACK_(n); # Funktion
                with_saved_context( funcall(fun,n); ); # Funktion aufrufen
                STACK_0 = value1; # Funktion im Stack durch den Wert ersetzen
              }}
              goto next_byte;
            CASE cod_apply:                  # (APPLY n)
              { var uintL n;
                U_operand(n);
               {var object fun = STACK_(n); # Funktion
                with_saved_context( apply(fun,n,value1); ); # Funktion aufrufen
                skipSTACK(1); # Funktion aus dem Stack streichen
              }}
              goto next_byte;
            CASE cod_apply_push:             # (APPLY&PUSH n)
              { var uintL n;
                U_operand(n);
               {var object fun = STACK_(n); # Funktion
                with_saved_context( apply(fun,n,value1); ); # Funktion aufrufen
                STACK_0 = value1; # Funktion im Stack durch den Wert ersetzen
              }}
              goto next_byte;
            # ------------------- (8) optionale und Keyword-Argumente -----------------------
            CASE cod_push_unbound:           # (PUSH-UNBOUND n)
              { var uintC n;
                U_operand(n);
                dotimesC(n,n, { pushSTACK(unbound); } );
              }
              goto next_byte;
            CASE cod_unlist:                 # (UNLIST n m)
              { var uintC n;
                var uintC m;
                U_operand(n);
                U_operand(m);
               {var object l = value1;
                if (n > 0)
                  do { if (atomp(l)) goto unlist_unbound;
                       pushSTACK(Car(l)); l = Cdr(l);
                     }
                     until (--n == 0);
                if (atomp(l))
                  goto next_byte;
                  else
                  fehler_apply_zuviel(S(lambda));
                unlist_unbound:
                if (n > m) fehler_apply_zuwenig(S(lambda));
                do { pushSTACK(unbound); } until (--n == 0);
                goto next_byte;
              }}
            CASE cod_unliststern:            # (UNLIST* n m)
              { var uintC n;
                var uintC m;
                U_operand(n);
                U_operand(m);
               {var object l = value1;
                do { if (atomp(l)) goto unliststern_unbound;
                     pushSTACK(Car(l)); l = Cdr(l);
                   }
                   until (--n == 0);
                pushSTACK(l);
                goto next_byte;
                unliststern_unbound:
                if (n > m) fehler_apply_zuwenig(S(lambda));
                do { pushSTACK(unbound); } until (--n == 0);
                pushSTACK(NIL);
                goto next_byte;
              }}
            CASE cod_jmpifboundp:            # (JMPIFBOUNDP n label)
              { var uintL n;
                U_operand(n);
               {var object obj = STACK_(n);
                if (eq(obj,unbound)) goto notjmp;
                value1 = obj; mv_count=1; JMP();
              }}
            CASE cod_boundp:                 # (BOUNDP n)
              { var uintL n;
                U_operand(n);
               {var object obj = STACK_(n);
                if (eq(obj,unbound)) goto code_nil; else goto code_t;
              }}
            CASE cod_unbound_nil:            # (UNBOUND->NIL n)
              { var uintL n;
                U_operand(n);
                if (eq(STACK_(n),unbound)) { STACK_(n) = NIL; }
              }
              goto next_byte;
            # ------------------- (9) Behandlung mehrerer Werte -----------------------
            CASE cod_values0:                # (VALUES0)
              value1 = NIL; mv_count = 0;
              goto next_byte;
            CASE cod_values1:                # (VALUES1)
              mv_count = 1;
              goto next_byte;
            CASE cod_stack_to_mv:            # (STACK-TO-MV n)
              { var uintL n;
                U_operand(n);
                if (n >= mv_limit) goto fehler_zuviele_werte;
                STACK_to_mv(n);
              }
              goto next_byte;
            CASE cod_mv_to_stack:            # (MV-TO-STACK)
              mv_to_STACK(); # Werte auf den Stack schieben
              goto next_byte;
            CASE cod_nv_to_stack:            # (NV-TO-STACK n)
              { var uintL n;
                U_operand(n);
                # Test auf Stacküberlauf:
                get_space_on_STACK(n*sizeof(object));
                # n Werte in den Stack schieben:
               {var uintC count = mv_count;
                if (n==0) goto nv_to_stack_end; # kein Wert gewünscht -> fertig
                # mindestens 1 Wert gewünscht
                pushSTACK(value1);
                n--; if (n==0) goto nv_to_stack_end; # nur 1 Wert gewünscht -> fertig
                if (count<=1) goto nv_to_stack_fill; # nur 1 Wert vorhanden -> mit NILs auffüllen
                count--;
                # mindestens 2 Werte gewünscht und vorhanden
                { var object* mvp = &mv_space[1];
                  loop
                    { pushSTACK(*mvp++);
                      n--; if (n==0) goto nv_to_stack_end; # kein Wert mehr gewünscht -> fertig
                      count--; if (count==0) goto nv_to_stack_fill; # kein Wert mehr vorhanden -> mit NILs auffüllen
                }   }
                nv_to_stack_fill: # Auffüllen mit n>0 NILs als zusätzlichen Werten:
                dotimespL(n,n, { pushSTACK(NIL); } );
                nv_to_stack_end: ;
              }}
              goto next_byte;
            CASE cod_mv_to_list:             # (MV-TO-LIST)
              with_saved_context(
                # Werte auf den Stack schieben und daraus Liste basteln:
                mv_to_list();
                );
              value1 = popSTACK(); mv_count=1;
              goto next_byte;
            CASE cod_list_to_mv:             # (LIST-TO-MV)
              list_to_mv(value1, { goto fehler_zuviele_werte; } );
              goto next_byte;
            CASE cod_mvcallp:                # (MVCALLP)
              pushSP((aint)STACK); # STACK retten
              pushSTACK(value1); # auszuführende Funktion retten
              goto next_byte;
            CASE cod_mvcall:                 # (MVCALL)
              { var object* FRAME; popSP( FRAME = (object*) ); # Pointer über Argumente und Funktion
               {var object fun = NEXT(FRAME); # Funktion
                with_saved_context(
                  {var uintL argcount = # Anzahl der Argumente auf dem Stack
                     STACK_item_count(STACK,FRAME);
                   if (((uintL)~(uintL)0 > ca_limit_1) && (argcount > ca_limit_1))
                     { pushSTACK(fun);
                       pushSTACK(S(multiple_value_call));
                       fehler(program_error,
                              GETTEXT("~: too many arguments given to ~")
                             );
                     }
                   # Funktion anwenden, Stack anheben bis unter die Funktion:
                   funcall(fun,argcount);
                   skipSTACK(1); # Funktion aus dem STACK streichen
                  });
              }}
              goto next_byte;
            # ------------------- (10) BLOCK -----------------------
            CASE cod_block_open:             # (BLOCK-OPEN n label)
              # belegt 3 STACK-Einträge und 1 SP-jmp_buf-Eintrag und 2 SP-Einträge
              { var uintL n;
                var sintL label_dist;
                U_operand(n);
                S_operand(label_dist);
                # Block_Cons erzeugen:
               {var object block_cons;
                with_saved_context(
                  block_cons = allocate_cons();
                  label_dist += index; # CODEPTR+label_dist ist das Sprungziel
                  );
                # Block-Cons füllen: (CONST n) als CAR
                Car(block_cons) = TheCclosure(closure)->clos_consts[n];
                # Sprungziel in den SP:
                pushSP(label_dist); pushSP((aint)closureptr);
                # CBLOCK-Frame aufbauen:
                { var object* top_of_frame = STACK; # Pointer übern Frame
                  pushSTACK(block_cons); # Cons ( (CONST n) . ...)
                 {var JMPBUF_on_SP(returner); # Rücksprungpunkt merken
                  finish_entry_frame_1(CBLOCK,returner, goto block_return; );
                }}
                # Framepointer im Block-Cons ablegen:
                Cdr(block_cons) = make_framepointer(STACK);
              }}
              goto next_byte;
              block_return: # Hierher wird gesprungen, wenn der oben aufgebaute
                            # CBLOCK-Frame ein RETURN-FROM gefangen hat.
              { FREE_JMPBUF_on_SP();
                skipSTACK(2); # CBLOCK-Frame auflösen, dabei
                Cdr(popSTACK()) = disabled; # Block-Cons als Disabled markieren
               {var uintL index;
                # closure zurück, byteptr:=label_byteptr :
                popSP(closureptr = (object*) ); popSP(index = );
                closure = *closureptr; # Closure aus dem Stack holen
                codeptr = TheSbvector(TheCclosure(closure)->clos_codevec);
                byteptr = CODEPTR + index;
              }}
              goto next_byte; # am Label weiterinterpretieren
            CASE cod_block_close:            # (BLOCK-CLOSE)
              # CBLOCK-Frame auflösen:
              #if STACKCHECKC
              if (!(framecode(STACK_0) == CBLOCK_frame_info))
                goto fehler_STACK_putt;
              #endif
              { FREE_JMPBUF_on_SP();
                skipSTACK(2); # CBLOCK-Frame auflösen, dabei
                Cdr(popSTACK()) = disabled; # Block-Cons als Disabled markieren
                skipSP(2); # Ziel-Closureptr und Ziel-Label kennen wir
              }
              goto next_byte; # am Label gleich weiterinterpretieren
            CASE cod_return_from:            # (RETURN-FROM n)
              { var uintL n;
                U_operand(n);
               {var object block_cons = TheCclosure(closure)->clos_consts[n];
                if (eq(Cdr(block_cons),disabled))
                  { fehler_block_left(Car(block_cons)); }
                # Bis zum Block-Frame unwinden, dann seine Routine zum Auflösen anspringen:
                #ifndef FAST_SP
                FREE_DYNAMIC_ARRAY(private_SP_space);
                #endif
                unwind_upto(uTheFramepointer(Cdr(block_cons)));
              }}
            CASE cod_return_from_i:          # (RETURN-FROM-I k1 k2 n)
              { var uintL k1;
                var uintL k2;
                var uintL n;
                U_operand(k1);
                U_operand(k2);
                U_operand(n);
               {var object* FRAME = (object*) SP_(k1+jmpbufsize*k2);
                var object block_cons = FRAME_(n);
                if (eq(Cdr(block_cons),disabled))
                  { fehler_block_left(Car(block_cons)); }
                # Bis zum Block-Frame unwinden, dann seine Routine zum Auflösen anspringen:
                #ifndef FAST_SP
                FREE_DYNAMIC_ARRAY(private_SP_space);
                #endif
                unwind_upto(uTheFramepointer(Cdr(block_cons)));
              }}
            # ------------------- (11) TAGBODY -----------------------
            CASE cod_tagbody_open:           # (TAGBODY-OPEN n label1 ... labelm)
              # belegt 3+m STACK-Einträge und 1 SP-jmp_buf-Eintrag und 1 SP-Eintrag
              { var uintL n;
                U_operand(n);
                # Tagbody-Cons erzeugen:
               {var object tagbody_cons;
                with_saved_context(
                  tagbody_cons = allocate_cons();
                  );
                # Tagbody-Cons füllen: Tag-Vektor (CONST n) als CAR
                {var object tag_vector = TheCclosure(closure)->clos_consts[n];
                 var uintL m = Svector_length(tag_vector);
                 Car(tagbody_cons) = tag_vector;
                 get_space_on_STACK(m*sizeof(object)); # Platz reservieren
                # alle labeli als Fixnums auf den STACK legen:
                 {var uintL count;
                  dotimespL(count,m,
                    { var const uintB* label_byteptr;
                      L_operand(label_byteptr);
                      pushSTACK(fixnum(label_byteptr - CODEPTR));
                    });
                }}
                # Sprungziel in den SP:
                pushSP((aint)closureptr);
                # CTAGBODY-Frame aufbauen:
                { var object* top_of_frame = STACK; # Pointer übern Frame
                  pushSTACK(tagbody_cons); # Cons ( (CONST n) . ...)
                 {var JMPBUF_on_SP(returner); # Rücksprungpunkt merken
                  finish_entry_frame_1(CTAGBODY,returner, goto tagbody_go; );
                }}
                # Framepointer im Tagbody-Cons ablegen:
                Cdr(tagbody_cons) = make_framepointer(STACK);
              }}
              goto next_byte;
              tagbody_go: # Hierher wird gesprungen, wenn der oben aufgebaute
                          # CTAGBODY-Frame ein GO zum Label Nummer i gefangen hat.
              { var uintL m = Svector_length(Car(STACK_2)); # Anzahl der Labels
                # (Könnte auch das obige m als 'auto' deklarieren und hier benutzen.)
                var uintL i = posfixnum_to_L(value1); # Nummer des Labels
                var uintL index = posfixnum_to_L(STACK_((m-i)+3)); # labeli
                # closure zurück, byteptr:=labeli_byteptr :
                closureptr = (object*) SP_(jmpbufsize+0);
                closure = *closureptr; # Closure aus dem Stack holen
                codeptr = TheSbvector(TheCclosure(closure)->clos_codevec);
                byteptr = CODEPTR + index;
              }
              goto next_byte; # am Label i weiterinterpretieren
            CASE cod_tagbody_close_nil:      # (TAGBODY-CLOSE-NIL)
              value1 = NIL; mv_count=1; # Wert des Tagbody ist NIL
            CASE cod_tagbody_close:          # (TAGBODY-CLOSE)
              # CTAGBODY-Frame auflösen:
              #if STACKCHECKC
              if (!(framecode(STACK_0) == CTAGBODY_frame_info))
                goto fehler_STACK_putt;
              #endif
              { FREE_JMPBUF_on_SP();
               {var object tagbody_cons = STACK_2; # Tagbody-Cons
                Cdr(tagbody_cons) = disabled; # als Disabled markieren
                skipSTACK(3+Svector_length(Car(tagbody_cons)));
                skipSP(1);
              }}
              goto next_byte;
            CASE cod_go:                     # (GO n l)
              { var uintL n;
                var uintL l;
                U_operand(n);
                U_operand(l);
               {var object tagbody_cons = # (CONST n)
                  TheCclosure(closure)->clos_consts[n];
                if (eq(Cdr(tagbody_cons),disabled))
                  { var object tag_vector = Car(tagbody_cons);
                    pushSTACK(tag_vector);
                    pushSTACK(TheSvector(tag_vector)->data[l]); # Marke l
                    pushSTACK(S(go));
                    fehler(control_error,
                           GETTEXT("(~ ~): the tagbody of the tags ~ has already been left")
                          );
                  }
                # Übergabewert an den Tagbody:
                # Bei CTAGBODY-Frames 1+l als Fixnum,
                # bei ITAGBODY-Frames die Formenliste zu Tag Nummer l.
                {var object* FRAME = uTheFramepointer(Cdr(tagbody_cons));
                 value1 = (framecode(FRAME_(0)) == CTAGBODY_frame_info
                           ? fixnum(1+l)
                           : FRAME_(frame_bindings+2*l+1)
                          );
                 mv_count=1;
                 # Bis zum Tagbody-Frame unwinden, dann seine Routine anspringen,
                 # die zum Label l springt:
                 #ifndef FAST_SP
                 FREE_DYNAMIC_ARRAY(private_SP_space);
                 #endif
                 unwind_upto(FRAME);
              }}}
            CASE cod_go_i:                   # (GO-I k1 k2 n l)
              { var uintL k1;
                var uintL k2;
                var uintL n;
                var uintL l;
                U_operand(k1);
                U_operand(k2);
                U_operand(n);
                U_operand(l);
               {var object* FRAME = (object*) SP_(k1+jmpbufsize*k2);
                var object tagbody_cons = FRAME_(n);
                if (eq(Cdr(tagbody_cons),disabled))
                  { var object tag_vector = Car(tagbody_cons);
                    pushSTACK(tag_vector);
                    pushSTACK(TheSvector(tag_vector)->data[l]); # Marke l
                    pushSTACK(S(go));
                    fehler(control_error,
                           GETTEXT("(~ ~): the tagbody of the tags ~ has already been left")
                          );
                  }
                # Übergabewert an den Tagbody:
                # Bei CTAGBODY-Frames 1+l als Fixnum.
                {var object* FRAME = uTheFramepointer(Cdr(tagbody_cons));
                 value1 = fixnum(1+l); mv_count=1;
                 # Bis zum Tagbody-Frame unwinden, dann seine Routine anspringen,
                 # die zum Label l springt:
                 #ifndef FAST_SP
                 FREE_DYNAMIC_ARRAY(private_SP_space);
                 #endif
                 unwind_upto(FRAME);
              }}}
            # ------------------- (12) CATCH und THROW -----------------------
            CASE cod_catch_open:             # (CATCH-OPEN label)
              # belegt 3 STACK-Einträge und 1 SP-jmp_buf-Eintrag und 2 SP-Einträge
              { var const uintB* label_byteptr;
                L_operand(label_byteptr);
                # closureptr, label_byteptr retten:
                pushSP(label_byteptr - CODEPTR); pushSP((aint)closureptr);
              } # Frame aufbauen:
              { var object* top_of_frame = STACK;
                pushSTACK(value1); # Tag
               {var JMPBUF_on_SP(returner); # Rücksprungpunkt merken
                finish_entry_frame_1(CATCH,returner, goto catch_return; );
              }}
              goto next_byte;
              catch_return: # Hierher wird gesprungen, wenn der oben aufgebaute
                            # Catch-Frame einen Throw gefangen hat.
              { FREE_JMPBUF_on_SP();
                skipSTACK(3); # CATCH-Frame auflösen
               {var uintL index;
                # closure zurück, byteptr:=label_byteptr :
                popSP(closureptr = (object*) ); popSP(index = );
                closure = *closureptr; # Closure aus dem Stack holen
                codeptr = TheSbvector(TheCclosure(closure)->clos_codevec);
                byteptr = CODEPTR + index;
              }}
              goto next_byte; # am Label weiterinterpretieren
            CASE cod_catch_close:            # (CATCH-CLOSE)
              # Es muss ein CATCH-Frame kommen:
              #if STACKCHECKC
              if (!(framecode(STACK_0) == CATCH_frame_info))
                goto fehler_STACK_putt;
              #endif
              FREE_JMPBUF_on_SP();
              #if STACKCHECKC
              if (!(closureptr == (object*)SP_(0))) # dort stehender Closureptr muss der jetzige sein
                goto fehler_STACK_putt;
              #endif
              skipSP(2); skipSTACK(3); # CATCH-Frame auflösen
              goto next_byte;
            CASE cod_throw:                  # (THROW)
              { var object tag = popSTACK();
                throw_to(tag);
                pushSTACK(tag);
                pushSTACK(S(throw));
                fehler(control_error,
                       GETTEXT("~: there is no CATCHer for tag ~")
                      );
              }
            # ------------------- (13) UNWIND-PROTECT -----------------------
            CASE cod_uwp_open:               # (UNWIND-PROTECT-OPEN label)
              # belegt 2 STACK-Einträge und 1 SP-jmp_buf-Eintrag und 2 SP-Einträge
              { var const uintB* label_byteptr;
                L_operand(label_byteptr);
                # closureptr, label_byteptr retten:
                pushSP(label_byteptr - CODEPTR); pushSP((aint)closureptr);
              } # Frame aufbauen:
              { var object* top_of_frame = STACK;
                var JMPBUF_on_SP(returner); # Rücksprungpunkt merken
                finish_entry_frame_1(UNWIND_PROTECT,returner, goto throw_save; );
              }
              goto next_byte;
              throw_save: # Hierher wird gesprungen, wenn der oben aufgebaute
                          # Unwind-Protect-Frame einen Throw aufgehalten hat.
              # unwind_protect_to_save ist zu retten und am Schluss anzuspringen.
              #if STACKCHECKC
              if (!(framecode(STACK_0) == UNWIND_PROTECT_frame_info))
                { fehler(serious_condition,
                         GETTEXT("STACK corrupted")
                        );
                }
              #endif
              # Frame auflösen:
              FREE_JMPBUF_on_SP();
              skipSTACK(2);
              { var uintL index;
                # closure zurück, byteptr:=label_byteptr :
                popSP(closureptr = (object*) );
                popSP(index = );
                # unwind_protect_to_save retten:
                pushSP((aint)unwind_protect_to_save.fun);
                pushSP((aint)unwind_protect_to_save.upto_frame);
                pushSP((aint)STACK); # Pointer übern Frame zusätzlich auf den SP
                # alle Werte auf den Stack:
                mv_to_STACK();
                # Cleanup-Formen ausführen:
                closure = *closureptr; # Closure aus dem Stack holen
                codeptr = TheSbvector(TheCclosure(closure)->clos_codevec);
                byteptr = CODEPTR + index;
              }
              goto next_byte;
            CASE cod_uwp_normal_exit:        # (UNWIND-PROTECT-NORMAL-EXIT)
              #if STACKCHECKC
              if (!(framecode(STACK_0) == UNWIND_PROTECT_frame_info))
                goto fehler_STACK_putt;
              if (!(closureptr == (object*)SP_(jmpbufsize+0))) # dort stehender Closureptr muss der jetzige sein
                goto fehler_STACK_putt;
              #endif
              # Frame auflösen:
              # Nichts zu tun, da closure und byteptr unverändert bleiben.
              FREE_JMPBUF_on_SP(); skipSP(2);
              skipSTACK(2);
              # Dummy-Werte für 'unwind_protect_to_save':
              pushSP((aint)NULL); pushSP((aint)NULL); # NULL,NULL -> uwp_continue
              pushSP((aint)STACK); # Pointer übern Frame zusätzlich auf den SP
              # alle Werte auf den Stack:
              mv_to_STACK();
              # Cleanup-Formen ausführen:
              goto next_byte;
            CASE cod_uwp_close:              # (UNWIND-PROTECT-CLOSE)
              # Hierher wird am Ende der Cleanup-Formen gesprungen.
              { var object* oldSTACK; # Wert von STACK vor dem Retten der Werte
                popSP( oldSTACK = (object*) );
               {var uintL mvcount = # Anzahl der geretteten Werte auf dem Stack
                  STACK_item_count(STACK,oldSTACK);
                if (mvcount >= mv_limit) goto fehler_zuviele_werte;
                STACK_to_mv(mvcount);
              }}
              # Rücksprung zum geretteten unwind_protect_to_save.fun :
              { var restart fun;
                var object* arg;
                popSP( arg = (object*) ); popSP( fun = (restart) );
                # Rücksprung zu uwp_continue oder uwp_jmpback oder unwind_upto:
                if (!(fun == (restart)NULL))
                  { (*fun)(arg); NOTREACHED } # Rücksprung zu unwind_upto o.ä.
                if (arg == (object*)NULL)
                  { # uwp_continue:
                    # Hierher wird gesprungen, wenn nach dem Ausführen der
                    # Cleanup-Formen einfach weiterinterpretiert werden soll.
                    goto next_byte;
                  }
                  else
                  { # uwp_jmpback:
                    # Hierher wird gesprungen, wenn nach dem Ausführen der
                    # Cleanup-Formen an der alten Stelle in derselben Closure
                    # weiterinterpretiert werden soll.
                    byteptr = CODEPTR + (uintP)arg;
                    goto next_byte;
              }   }
            CASE cod_uwp_cleanup:            # (UNWIND-PROTECT-CLEANUP)
              # Dies wird ausgeführt, wenn innerhalb derselben Closure ein
              # Ausführen des Cleanup-Codes nötig ist.
              #if STACKCHECKC
              if (!(framecode(STACK_0) == UNWIND_PROTECT_frame_info))
                goto fehler_STACK_putt;
              if (!(closureptr == (object*)SP_(jmpbufsize+0))) # dort stehender Closureptr muss der jetzige sein
                goto fehler_STACK_putt;
              #endif
              # closure bleibt, byteptr:=label_byteptr :
              { var uintL index = SP_(jmpbufsize+1);
                # Frame auflösen:
                FREE_JMPBUF_on_SP(); skipSP(2);
                skipSTACK(2);
                # Dummy-Werte für 'unwind_protect_to_save':
                pushSP((aint)NULL); # NULL -> uwp_jmpback
                pushSP(byteptr - CODEPTR);
                pushSP((aint)STACK); # Pointer übern Frame zusätzlich auf den SP
                # alle Werte auf den Stack:
                mv_to_STACK();
                # Cleanup-Formen ausführen:
                byteptr = CODEPTR + index;
              }
              goto next_byte;
            # ------------------- (14) HANDLER-BIND -----------------------
            CASE cod_handler_open:           # (HANDLER-OPEN n)
              # belegt 4 STACK-Einträge
              { var uintL n;
                U_operand(n);
                # Frame aufbauen:
               {var object* top_of_frame = STACK; # Pointer übern Frame
                pushSTACK(TheCclosure(closure)->clos_consts[n]);
                pushSTACK(closure);
                pushSTACK(as_object((aint)(_SP_(0))));
                finish_frame(HANDLER);
              }}
              goto next_byte;
            CASE cod_handler_begin_push:     # (HANDLER-BEGIN&PUSH)
              # baut SP neu auf, belegt 1 SP-Eintrag und
              # beginnt einen neuen STACK-Bereich.
              { var uintL count = posfixnum_to_L(Car(handler_args.spdepth))
                                  + jmpbufsize * posfixnum_to_L(Cdr(handler_args.spdepth));
                if (count > 0)
                  { var SPint* oldsp = handler_args.sp; # war früher &SP_(0)
                    # oldsp[0..count-1] auf den jetzigen SP kopieren:
                    oldsp skipSPop count;
                    dotimespL(count,count, { oldsp skipSPop -1; pushSP(*oldsp); } );
              }   }
              pushSP((aint)handler_args.stack); # Pointer übern Handler-Frame
              value1 = handler_args.condition; mv_count=1;
              pushSTACK(value1);
              goto next_byte;
            # ------------------- (15) einige Funktionen -----------------------
            CASE cod_not:                    # (NOT)
              if (nullp(value1)) goto code_t; else goto code_nil;
            CASE cod_eq:                     # (EQ)
              if (!eq(value1,popSTACK())) goto code_nil; else goto code_t;
            CASE cod_car:                    # (CAR)
              { var object arg = value1;
                if (consp(arg)) { value1 = Car(arg); } # CAR eines Cons
                elif (nullp(arg)) {} # (CAR NIL) = NIL: value1 bleibt NIL
                else { subr_self = L(car); fehler_list(arg); }
                mv_count=1;
              }
              goto next_byte;
            CASE cod_car_push:               # (CAR&PUSH)
              { var object arg = value1;
                if (consp(arg)) { pushSTACK(Car(arg)); } # CAR eines Cons
                elif (nullp(arg)) { pushSTACK(arg); } # (CAR NIL) = NIL
                else { subr_self = L(car); fehler_list(arg); }
              }
              goto next_byte;
            CASE cod_load_car_push:          # (LOAD&CAR&PUSH n)
              { var uintL n;
                U_operand(n);
               {var object arg = STACK_(n);
                if (consp(arg)) { pushSTACK(Car(arg)); } # CAR eines Cons
                elif (nullp(arg)) { pushSTACK(arg); } # (CAR NIL) = NIL
                else { subr_self = L(car); fehler_list(arg); }
              }}
              goto next_byte;
            CASE cod_load_car_store:         # (LOAD&CAR&STORE m n)
              { var uintL m;
                var uintL n;
                U_operand(m);
                U_operand(n);
               {var object arg = STACK_(m);
                if (consp(arg)) { STACK_(n) = value1 = Car(arg); } # CAR eines Cons
                elif (nullp(arg)) { STACK_(n) = value1 = arg; } # (CAR NIL) = NIL
                else { subr_self = L(car); fehler_list(arg); }
                mv_count=1;
              }}
              goto next_byte;
            CASE cod_cdr:                    # (CDR)
              { var object arg = value1;
                if (consp(arg)) { value1 = Cdr(arg); } # CDR eines Cons
                elif (nullp(arg)) {} # (CDR NIL) = NIL: value1 bleibt NIL
                else { subr_self = L(cdr); fehler_list(arg); }
                mv_count=1;
              }
              goto next_byte;
            CASE cod_cdr_push:               # (CDR&PUSH)
              { var object arg = value1;
                if (consp(arg)) { pushSTACK(Cdr(arg)); } # CDR eines Cons
                elif (nullp(arg)) { pushSTACK(arg); } # (CDR NIL) = NIL
                else { subr_self = L(cdr); fehler_list(arg); }
              }
              goto next_byte;
            CASE cod_load_cdr_push:          # (LOAD&CDR&PUSH n)
              { var uintL n;
                U_operand(n);
               {var object arg = STACK_(n);
                if (consp(arg)) { pushSTACK(Cdr(arg)); } # CDR eines Cons
                elif (nullp(arg)) { pushSTACK(arg); } # (CDR NIL) = NIL
                else { subr_self = L(cdr); fehler_list(arg); }
              }}
              goto next_byte;
            CASE cod_load_cdr_store:         # (LOAD&CDR&STORE n)
              { var uintL n;
                U_operand(n);
               {var object* arg_ = &STACK_(n);
                var object arg = *arg_;
                if (consp(arg)) { *arg_ = value1 = Cdr(arg); } # CDR eines Cons
                elif (nullp(arg)) { value1 = arg; } # (CDR NIL) = NIL
                else { subr_self = L(cdr); fehler_list(arg); }
                mv_count=1;
              }}
              goto next_byte;
            CASE cod_cons:                   # (CONS)
              pushSTACK(value1);
              # Cons anfordern:
              {var object new_cons;
               with_saved_context( { new_cons = allocate_cons(); } );
               # Cons füllen:
               Cdr(new_cons) = popSTACK();
               Car(new_cons) = popSTACK();
               value1 = new_cons; mv_count=1;
              }
              goto next_byte;
            CASE cod_cons_push:              # (CONS&PUSH)
              pushSTACK(value1);
              # Cons anfordern:
              {var object new_cons;
               with_saved_context( { new_cons = allocate_cons(); } );
               # Cons füllen:
               Cdr(new_cons) = popSTACK();
               Car(new_cons) = STACK_0;
               STACK_0 = new_cons;
              }
              goto next_byte;
            CASE cod_load_cons_store:        # (LOAD&CONS&STORE n)
              { var uintL n;
                U_operand(n);
                # Cons anfordern:
               {var object new_cons;
                with_saved_context( { new_cons = allocate_cons(); } );
                # Cons füllen:
                Car(new_cons) = popSTACK();
                {var object* arg_ = &STACK_(n);
                 Cdr(new_cons) = *arg_;
                 value1 = *arg_ = new_cons; mv_count=1;
              }}}
              goto next_byte;
            {var object symbol;
            CASE cod_symbol_function:        # (SYMBOL-FUNCTION)
              symbol = value1;
              if (!symbolp(symbol)) goto csf_kein_symbol;
              if (eq(Symbol_function(symbol),unbound)) goto csf_unbound;
              value1 = Symbol_function(symbol); mv_count=1;
              goto next_byte;
            CASE cod_const_symbol_function:  # (CONST&SYMBOL-FUNCTION n)
              {var uintL n;
               U_operand(n);
               symbol = TheCclosure(closure)->clos_consts[n];
              }
              if (!symbolp(symbol)) goto csf_kein_symbol;
              if (eq(Symbol_function(symbol),unbound)) goto csf_unbound;
              value1 = Symbol_function(symbol); mv_count=1;
              goto next_byte;
            CASE cod_const_symbol_function_push: # (CONST&SYMBOL-FUNCTION&PUSH n)
              {var uintL n;
               U_operand(n);
               symbol = TheCclosure(closure)->clos_consts[n];
              }
              if (!symbolp(symbol)) goto csf_kein_symbol;
              if (eq(Symbol_function(symbol),unbound)) goto csf_unbound;
              pushSTACK(Symbol_function(symbol));
              goto next_byte;
            CASE cod_const_symbol_function_store: # (CONST&SYMBOL-FUNCTION&STORE n k)
              {var uintL n;
               U_operand(n);
               symbol = TheCclosure(closure)->clos_consts[n];
              }
              if (!symbolp(symbol)) goto csf_kein_symbol;
              if (eq(Symbol_function(symbol),unbound)) goto csf_unbound;
              {var uintL k;
               U_operand(k);
               STACK_(k) = value1 = Symbol_function(symbol); mv_count=1;
              }
              goto next_byte;
            csf_kein_symbol:
              fehler_kein_symbol(S(symbol_function),symbol);
            csf_unbound:
              # (symbol zwar evtl. nicht der eigentliche Funktionsname, denn
              # z.B. (FUNCTION FOO) wird in (SYMBOL-FUNCTION '#:|(SETF FOO)|)
              # umgewandelt, aber für die Fehlermeldung reicht das wohl.)
              fehler_undefined(S(symbol_function),symbol);
            }
            {var object vec; var object index;
            CASE cod_svref:                  # (SVREF)
              # STACK_0 muss ein Simple-Vector sein:
              if (!simple_vector_p(STACK_0)) goto svref_kein_svector;
              vec = popSTACK(); # Simple-Vector
              index = value1;
              # und der Index muss ein Fixnum >=0, <Länge(vec) sein:
              {var uintL i;
               if (!(posfixnump(index) &&
                     ((i = posfixnum_to_L(index)) < Svector_length(vec))
                  ) )
                 goto svref_kein_index;
               value1 = TheSvector(vec)->data[i]; # indiziertes Element als Wert
               mv_count = 1;
              }
              goto next_byte;
            CASE cod_svset:                  # (SVSET)
              # STACK_0 muss ein Simple-Vector sein:
              if (!simple_vector_p(STACK_0)) goto svref_kein_svector;
              vec = popSTACK(); # Simple-Vector
              index = value1;
              # und der Index muss ein Fixnum >=0, <Länge(vec) sein:
              {var uintL i;
               if (!(posfixnump(index) &&
                     ((i = posfixnum_to_L(index)) < Svector_length(vec))
                  ) )
                 goto svref_kein_index;
               value1 = TheSvector(vec)->data[i] = popSTACK(); # neues Element hineinstecken
               mv_count = 1;
              }
              goto next_byte;
            svref_kein_svector: # Nicht-Simple-Vector in STACK_0
              fehler_kein_svector(S(svref),STACK_0);
            svref_kein_index: # unpassender Index in index, zum Vektor vec
              pushSTACK(vec);
              pushSTACK(index);
              pushSTACK(index); # Wert für Slot DATUM von TYPE-ERROR
              { var object tmp;
                pushSTACK(S(integer)); pushSTACK(Fixnum_0); pushSTACK(UL_to_I(Svector_length(vec)));
                tmp = listof(1); pushSTACK(tmp); tmp = listof(3);
                pushSTACK(tmp); # Wert für Slot EXPECTED-TYPE von TYPE-ERROR
              }
              pushSTACK(STACK_(1+2)); # vec
              pushSTACK(STACK_(0+3)); # index
              pushSTACK(S(svref));
              fehler(type_error,
                     GETTEXT("~: ~ is not a correct index into ~")
                    );
            }
            CASE cod_list:                   # (LIST n)
              { var uintC n;
                U_operand(n);
                with_saved_context( { value1 = listof(n); mv_count=1; } );
              }
              goto next_byte;
            CASE cod_list_push:              # (LIST&PUSH n)
              { var uintC n;
                U_operand(n);
                with_saved_context( { pushSTACK(listof(n)); } );
              }
              goto next_byte;
            CASE cod_liststern:              # (LIST* n)
              { var uintC n;
                U_operand(n);
                with_saved_context(
                  { pushSTACK(value1);
                    dotimespC(n,n,
                      { var object new_cons = allocate_cons();
                        Cdr(new_cons) = popSTACK();
                        Car(new_cons) = STACK_0;
                        STACK_0 = new_cons;
                      });
                    value1 = popSTACK(); mv_count=1;
                  });
              }
              goto next_byte;
            CASE cod_liststern_push:         # (LIST*&PUSH n)
              { var uintC n;
                U_operand(n);
                with_saved_context(
                  { pushSTACK(value1);
                    dotimespC(n,n,
                      { var object new_cons = allocate_cons();
                        Cdr(new_cons) = popSTACK();
                        Car(new_cons) = STACK_0;
                        STACK_0 = new_cons;
                      });
                  });
              }
              goto next_byte;
            # ------------------- (16) kombinierte Operationen -----------------------
            CASE cod_nil_store:              # (NIL&STORE n)
              {var uintL n;
               U_operand(n);
               STACK_(n) = value1 = NIL; mv_count=1;
              }
              goto next_byte;
            CASE cod_t_store:                # (T&STORE n)
              {var uintL n;
               U_operand(n);
               STACK_(n) = value1 = T; mv_count=1;
              }
              goto next_byte;
            CASE cod_calls1_store:           # (CALLS1&STORE n k)
              CALLS1();
              goto store;
            CASE cod_calls2_store:           # (CALLS2&STORE n k)
              CALLS2();
              goto store;
            CASE cod_callsr_store:           # (CALLSR&STORE m n k)
              CALLSR();
              goto store;
            # Incrementieren. Speziell optimiert für Fixnums >=0.
            #define INC(arg,statement)  \
              { if (posfixnump(arg) # Fixnum >= 0 und < most-positive-fixnum ? \
                    && !eq(arg,fixnum(bitm(oint_data_len)-1))                  \
                   )                                                           \
                  { arg = fixnum_inc(arg,1); statement; }                      \
                  else                                                         \
                  { with_saved_context(                                        \
                      { pushSTACK(arg); subr_self = L(einsplus); C_einsplus(); } # funcall(L(einsplus),1); \
                      );                                                       \
                    arg = value1;                                              \
              }   }
            # Decrementieren. Speziell optimiert für Fixnums >=0.
            #define DEC(arg,statement)  \
              { if (posfixnump(arg) && !eq(arg,Fixnum_0)) # Fixnum > 0 ? \
                  { arg = fixnum_inc(arg,-1); statement; }               \
                  else                                                   \
                  { with_saved_context(                                  \
                      { pushSTACK(arg); subr_self = L(einsminus); C_einsminus(); } # funcall(L(einsminus),1); \
                      );                                                 \
                    arg = value1;                                        \
              }   }
            CASE cod_load_inc_push:          # (LOAD&INC&PUSH n)
              { var uintL n;
                U_operand(n);
               {var object arg = STACK_(n);
                INC(arg,); # incrementieren
                pushSTACK(arg);
              }}
              goto next_byte;
            CASE cod_load_inc_store:         # (LOAD&INC&STORE n)
              { var uintL n;
                U_operand(n);
               {var object* arg_ = &STACK_(n);
                var object arg = *arg_;
                INC(arg,mv_count=1); # incrementieren, 1 Wert
                value1 = *arg_ = arg;
              }}
              goto next_byte;
            CASE cod_load_dec_push:          # (LOAD&DEC&PUSH n)
              { var uintL n;
                U_operand(n);
               {var object arg = STACK_(n);
                DEC(arg,); # decrementieren
                pushSTACK(arg);
              }}
              goto next_byte;
            CASE cod_load_dec_store:         # (LOAD&DEC&STORE n)
              { var uintL n;
                U_operand(n);
               {var object* arg_ = &STACK_(n);
                var object arg = *arg_;
                DEC(arg,mv_count=1); # decrementieren, 1 Wert
                value1 = *arg_ = arg;
              }}
              goto next_byte;
            CASE cod_call1_jmpif:            # (CALL1&JMPIF n label)
              CALL1();
              if (!nullp(value1)) goto jmp; else goto notjmp;
            CASE cod_call1_jmpifnot:         # (CALL1&JMPIFNOT n label)
              CALL1();
              if (nullp(value1)) goto jmp; else goto notjmp;
            CASE cod_call2_jmpif:            # (CALL2&JMPIF n label)
              CALL2();
              if (!nullp(value1)) goto jmp; else goto notjmp;
            CASE cod_call2_jmpifnot:         # (CALL2&JMPIFNOT n label)
              CALL2();
              if (nullp(value1)) goto jmp; else goto notjmp;
            CASE cod_calls1_jmpif:           # (CALLS1&JMPIF n label)
              CALLS1();
              if (!nullp(value1)) goto jmp; else goto notjmp;
            CASE cod_calls1_jmpifnot:        # (CALLS1&JMPIFNOT n label)
              CALLS1();
              if (nullp(value1)) goto jmp; else goto notjmp;
            CASE cod_calls2_jmpif:           # (CALLS2&JMPIF n label)
              CALLS2();
              if (!nullp(value1)) goto jmp; else goto notjmp;
            CASE cod_calls2_jmpifnot:        # (CALLS2&JMPIFNOT n label)
              CALLS2();
              if (nullp(value1)) goto jmp; else goto notjmp;
            CASE cod_callsr_jmpif:           # (CALLSR&JMPIF m n label)
              CALLSR();
              if (!nullp(value1)) goto jmp; else goto notjmp;
            CASE cod_callsr_jmpifnot:        # (CALLSR&JMPIFNOT m n label)
              CALLSR();
              if (nullp(value1)) goto jmp; else goto notjmp;
            CASE cod_load_jmpif:             # (LOAD&JMPIF n label)
              {var uintL n;
               U_operand(n);
               mv_count=1;
               if (!nullp(value1 = STACK_(n))) goto jmp; else goto notjmp;
              }
            CASE cod_load_jmpifnot:          # (LOAD&JMPIFNOT n label)
              {var uintL n;
               U_operand(n);
               mv_count=1;
               if (nullp(value1 = STACK_(n))) goto jmp; else goto notjmp;
              }
            CASE cod_apply_skip_ret:         # (APPLY&SKIP&RET n k)
              { var uintL n;
                var uintL k;
                U_operand(n);
                U_operand(k);
               {var object fun = STACK_(n); # Funktion
                with_saved_context(
                  { apply(fun,n,value1); # Funktion aufrufen
                    skipSTACK(k+1); # Funktion u.a. aus dem Stack streichen
                    goto finished; # Rücksprung zum Aufrufer
                  }); # der Kontext wird nicht restauriert
              }}
            CASE cod_funcall_skip_retgf:     # (FUNCALL&SKIP&RETGF n k)
              { var uintL n;
                var uintL k;
                U_operand(n);
                U_operand(k);
               {var object fun = STACK_(n); # Funktion
                var uintL r = ((Codevec)codeptr)->ccv_numreq;
                var uintB flags = ((Codevec)codeptr)->ccv_flags;
                with_saved_context(
                  { funcall(fun,n); # Funktion aufrufen
                    k -= r;
                    if (flags & bit(0))
                      { skipSTACK(k); apply(value1,r,popSTACK()); }
                    else
                      { skipSTACK(k+1); funcall(value1,r); }
                    goto finished; # Rücksprung zum Aufrufer
                  }); # der Kontext wird nicht restauriert
              }}
            # ------------------- (17) Kurzcodes -----------------------
            CASE cod_load0:                  # (LOAD.S 0)
              value1 = STACK_(0); mv_count=1;
              goto next_byte;
            CASE cod_load1:                  # (LOAD.S 1)
              value1 = STACK_(1); mv_count=1;
              goto next_byte;
            CASE cod_load2:                  # (LOAD.S 2)
              value1 = STACK_(2); mv_count=1;
              goto next_byte;
            CASE cod_load3:                  # (LOAD.S 3)
              value1 = STACK_(3); mv_count=1;
              goto next_byte;
            CASE cod_load4:                  # (LOAD.S 4)
              value1 = STACK_(4); mv_count=1;
              goto next_byte;
            CASE cod_load5:                  # (LOAD.S 5)
              value1 = STACK_(5); mv_count=1;
              goto next_byte;
            CASE cod_load6:                  # (LOAD.S 6)
              value1 = STACK_(6); mv_count=1;
              goto next_byte;
            CASE cod_load7:                  # (LOAD.S 7)
              value1 = STACK_(7); mv_count=1;
              goto next_byte;
            CASE cod_load8:                  # (LOAD.S 8)
              value1 = STACK_(8); mv_count=1;
              goto next_byte;
            CASE cod_load9:                  # (LOAD.S 9)
              value1 = STACK_(9); mv_count=1;
              goto next_byte;
            CASE cod_load10:                 # (LOAD.S 10)
              value1 = STACK_(10); mv_count=1;
              goto next_byte;
            CASE cod_load11:                 # (LOAD.S 11)
              value1 = STACK_(11); mv_count=1;
              goto next_byte;
            CASE cod_load12:                 # (LOAD.S 12)
              value1 = STACK_(12); mv_count=1;
              goto next_byte;
            CASE cod_load13:                 # (LOAD.S 13)
              value1 = STACK_(13); mv_count=1;
              goto next_byte;
            CASE cod_load14:                 # (LOAD.S 14)
              value1 = STACK_(14); mv_count=1;
              goto next_byte;
            #if 0
            CASE cod_load15:                 # (LOAD.S 15)
              value1 = STACK_(15); mv_count=1;
              goto next_byte;
            CASE cod_load16:                 # (LOAD.S 16)
              value1 = STACK_(16); mv_count=1;
              goto next_byte;
            CASE cod_load17:                 # (LOAD.S 17)
              value1 = STACK_(17); mv_count=1;
              goto next_byte;
            CASE cod_load18:                 # (LOAD.S 18)
              value1 = STACK_(18); mv_count=1;
              goto next_byte;
            CASE cod_load19:                 # (LOAD.S 19)
              value1 = STACK_(19); mv_count=1;
              goto next_byte;
            CASE cod_load20:                 # (LOAD.S 20)
              value1 = STACK_(20); mv_count=1;
              goto next_byte;
            CASE cod_load21:                 # (LOAD.S 21)
              value1 = STACK_(21); mv_count=1;
              goto next_byte;
            #endif
            CASE cod_load_push0:             # (LOAD&PUSH.S 0)
              pushSTACK(STACK_(0));
              goto next_byte;
            CASE cod_load_push1:             # (LOAD&PUSH.S 1)
              pushSTACK(STACK_(1));
              goto next_byte;
            CASE cod_load_push2:             # (LOAD&PUSH.S 2)
              pushSTACK(STACK_(2));
              goto next_byte;
            CASE cod_load_push3:             # (LOAD&PUSH.S 3)
              pushSTACK(STACK_(3));
              goto next_byte;
            CASE cod_load_push4:             # (LOAD&PUSH.S 4)
              pushSTACK(STACK_(4));
              goto next_byte;
            CASE cod_load_push5:             # (LOAD&PUSH.S 5)
              pushSTACK(STACK_(5));
              goto next_byte;
            CASE cod_load_push6:             # (LOAD&PUSH.S 6)
              pushSTACK(STACK_(6));
              goto next_byte;
            CASE cod_load_push7:             # (LOAD&PUSH.S 7)
              pushSTACK(STACK_(7));
              goto next_byte;
            CASE cod_load_push8:             # (LOAD&PUSH.S 8)
              pushSTACK(STACK_(8));
              goto next_byte;
            CASE cod_load_push9:             # (LOAD&PUSH.S 9)
              pushSTACK(STACK_(9));
              goto next_byte;
            CASE cod_load_push10:            # (LOAD&PUSH.S 10)
              pushSTACK(STACK_(10));
              goto next_byte;
            CASE cod_load_push11:            # (LOAD&PUSH.S 11)
              pushSTACK(STACK_(11));
              goto next_byte;
            CASE cod_load_push12:            # (LOAD&PUSH.S 12)
              pushSTACK(STACK_(12));
              goto next_byte;
            CASE cod_load_push13:            # (LOAD&PUSH.S 13)
              pushSTACK(STACK_(13));
              goto next_byte;
            CASE cod_load_push14:            # (LOAD&PUSH.S 14)
              pushSTACK(STACK_(14));
              goto next_byte;
            CASE cod_load_push15:            # (LOAD&PUSH.S 15)
              pushSTACK(STACK_(15));
              goto next_byte;
            CASE cod_load_push16:            # (LOAD&PUSH.S 16)
              pushSTACK(STACK_(16));
              goto next_byte;
            CASE cod_load_push17:            # (LOAD&PUSH.S 17)
              pushSTACK(STACK_(17));
              goto next_byte;
            CASE cod_load_push18:            # (LOAD&PUSH.S 18)
              pushSTACK(STACK_(18));
              goto next_byte;
            CASE cod_load_push19:            # (LOAD&PUSH.S 19)
              pushSTACK(STACK_(19));
              goto next_byte;
            CASE cod_load_push20:            # (LOAD&PUSH.S 20)
              pushSTACK(STACK_(20));
              goto next_byte;
            CASE cod_load_push21:            # (LOAD&PUSH.S 21)
              pushSTACK(STACK_(21));
              goto next_byte;
            CASE cod_load_push22:            # (LOAD&PUSH.S 22)
              pushSTACK(STACK_(22));
              goto next_byte;
            CASE cod_load_push23:            # (LOAD&PUSH.S 23)
              pushSTACK(STACK_(23));
              goto next_byte;
            CASE cod_load_push24:            # (LOAD&PUSH.S 24)
              pushSTACK(STACK_(24));
              goto next_byte;
            CASE cod_const0:                 # (CONST.S 0)
              value1 = TheCclosure(closure)->clos_consts[0]; mv_count=1;
              goto next_byte;
            CASE cod_const1:                 # (CONST.S 1)
              value1 = TheCclosure(closure)->clos_consts[1]; mv_count=1;
              goto next_byte;
            CASE cod_const2:                 # (CONST.S 2)
              value1 = TheCclosure(closure)->clos_consts[2]; mv_count=1;
              goto next_byte;
            CASE cod_const3:                 # (CONST.S 3)
              value1 = TheCclosure(closure)->clos_consts[3]; mv_count=1;
              goto next_byte;
            CASE cod_const4:                 # (CONST.S 4)
              value1 = TheCclosure(closure)->clos_consts[4]; mv_count=1;
              goto next_byte;
            CASE cod_const5:                 # (CONST.S 5)
              value1 = TheCclosure(closure)->clos_consts[5]; mv_count=1;
              goto next_byte;
            CASE cod_const6:                 # (CONST.S 6)
              value1 = TheCclosure(closure)->clos_consts[6]; mv_count=1;
              goto next_byte;
            CASE cod_const7:                 # (CONST.S 7)
              value1 = TheCclosure(closure)->clos_consts[7]; mv_count=1;
              goto next_byte;
            CASE cod_const8:                 # (CONST.S 8)
              value1 = TheCclosure(closure)->clos_consts[8]; mv_count=1;
              goto next_byte;
            CASE cod_const9:                 # (CONST.S 9)
              value1 = TheCclosure(closure)->clos_consts[9]; mv_count=1;
              goto next_byte;
            CASE cod_const10:                # (CONST.S 10)
              value1 = TheCclosure(closure)->clos_consts[10]; mv_count=1;
              goto next_byte;
            CASE cod_const11:                # (CONST.S 11)
              value1 = TheCclosure(closure)->clos_consts[11]; mv_count=1;
              goto next_byte;
            CASE cod_const12:                # (CONST.S 12)
              value1 = TheCclosure(closure)->clos_consts[12]; mv_count=1;
              goto next_byte;
            CASE cod_const13:                # (CONST.S 13)
              value1 = TheCclosure(closure)->clos_consts[13]; mv_count=1;
              goto next_byte;
            CASE cod_const14:                # (CONST.S 14)
              value1 = TheCclosure(closure)->clos_consts[14]; mv_count=1;
              goto next_byte;
            CASE cod_const15:                # (CONST.S 15)
              value1 = TheCclosure(closure)->clos_consts[15]; mv_count=1;
              goto next_byte;
            CASE cod_const16:                # (CONST.S 16)
              value1 = TheCclosure(closure)->clos_consts[16]; mv_count=1;
              goto next_byte;
            CASE cod_const17:                # (CONST.S 17)
              value1 = TheCclosure(closure)->clos_consts[17]; mv_count=1;
              goto next_byte;
            CASE cod_const18:                # (CONST.S 18)
              value1 = TheCclosure(closure)->clos_consts[18]; mv_count=1;
              goto next_byte;
            CASE cod_const19:                # (CONST.S 19)
              value1 = TheCclosure(closure)->clos_consts[19]; mv_count=1;
              goto next_byte;
            CASE cod_const20:                # (CONST.S 20)
              value1 = TheCclosure(closure)->clos_consts[20]; mv_count=1;
              goto next_byte;
            #if 0
            CASE cod_const21:                # (CONST.S 21)
              value1 = TheCclosure(closure)->clos_consts[21]; mv_count=1;
              goto next_byte;
            CASE cod_const22:                # (CONST.S 22)
              value1 = TheCclosure(closure)->clos_consts[22]; mv_count=1;
              goto next_byte;
            CASE cod_const23:                # (CONST.S 23)
              value1 = TheCclosure(closure)->clos_consts[23]; mv_count=1;
              goto next_byte;
            CASE cod_const24:                # (CONST.S 24)
              value1 = TheCclosure(closure)->clos_consts[24]; mv_count=1;
              goto next_byte;
            #endif
            CASE cod_const_push0:            # (CONST&PUSH.S 0)
              pushSTACK(TheCclosure(closure)->clos_consts[0]);
              goto next_byte;
            CASE cod_const_push1:            # (CONST&PUSH.S 1)
              pushSTACK(TheCclosure(closure)->clos_consts[1]);
              goto next_byte;
            CASE cod_const_push2:            # (CONST&PUSH.S 2)
              pushSTACK(TheCclosure(closure)->clos_consts[2]);
              goto next_byte;
            CASE cod_const_push3:            # (CONST&PUSH.S 3)
              pushSTACK(TheCclosure(closure)->clos_consts[3]);
              goto next_byte;
            CASE cod_const_push4:            # (CONST&PUSH.S 4)
              pushSTACK(TheCclosure(closure)->clos_consts[4]);
              goto next_byte;
            CASE cod_const_push5:            # (CONST&PUSH.S 5)
              pushSTACK(TheCclosure(closure)->clos_consts[5]);
              goto next_byte;
            CASE cod_const_push6:            # (CONST&PUSH.S 6)
              pushSTACK(TheCclosure(closure)->clos_consts[6]);
              goto next_byte;
            CASE cod_const_push7:            # (CONST&PUSH.S 7)
              pushSTACK(TheCclosure(closure)->clos_consts[7]);
              goto next_byte;
            CASE cod_const_push8:            # (CONST&PUSH.S 8)
              pushSTACK(TheCclosure(closure)->clos_consts[8]);
              goto next_byte;
            CASE cod_const_push9:            # (CONST&PUSH.S 9)
              pushSTACK(TheCclosure(closure)->clos_consts[9]);
              goto next_byte;
            CASE cod_const_push10:           # (CONST&PUSH.S 10)
              pushSTACK(TheCclosure(closure)->clos_consts[10]);
              goto next_byte;
            CASE cod_const_push11:           # (CONST&PUSH.S 11)
              pushSTACK(TheCclosure(closure)->clos_consts[11]);
              goto next_byte;
            CASE cod_const_push12:           # (CONST&PUSH.S 12)
              pushSTACK(TheCclosure(closure)->clos_consts[12]);
              goto next_byte;
            CASE cod_const_push13:           # (CONST&PUSH.S 13)
              pushSTACK(TheCclosure(closure)->clos_consts[13]);
              goto next_byte;
            CASE cod_const_push14:           # (CONST&PUSH.S 14)
              pushSTACK(TheCclosure(closure)->clos_consts[14]);
              goto next_byte;
            CASE cod_const_push15:           # (CONST&PUSH.S 15)
              pushSTACK(TheCclosure(closure)->clos_consts[15]);
              goto next_byte;
            CASE cod_const_push16:           # (CONST&PUSH.S 16)
              pushSTACK(TheCclosure(closure)->clos_consts[16]);
              goto next_byte;
            CASE cod_const_push17:           # (CONST&PUSH.S 17)
              pushSTACK(TheCclosure(closure)->clos_consts[17]);
              goto next_byte;
            CASE cod_const_push18:           # (CONST&PUSH.S 18)
              pushSTACK(TheCclosure(closure)->clos_consts[18]);
              goto next_byte;
            CASE cod_const_push19:           # (CONST&PUSH.S 19)
              pushSTACK(TheCclosure(closure)->clos_consts[19]);
              goto next_byte;
            CASE cod_const_push20:           # (CONST&PUSH.S 20)
              pushSTACK(TheCclosure(closure)->clos_consts[20]);
              goto next_byte;
            CASE cod_const_push21:           # (CONST&PUSH.S 21)
              pushSTACK(TheCclosure(closure)->clos_consts[21]);
              goto next_byte;
            CASE cod_const_push22:           # (CONST&PUSH.S 22)
              pushSTACK(TheCclosure(closure)->clos_consts[22]);
              goto next_byte;
            CASE cod_const_push23:           # (CONST&PUSH.S 23)
              pushSTACK(TheCclosure(closure)->clos_consts[23]);
              goto next_byte;
            CASE cod_const_push24:           # (CONST&PUSH.S 24)
              pushSTACK(TheCclosure(closure)->clos_consts[24]);
              goto next_byte;
            CASE cod_const_push25:           # (CONST&PUSH.S 25)
              pushSTACK(TheCclosure(closure)->clos_consts[25]);
              goto next_byte;
            CASE cod_const_push26:           # (CONST&PUSH.S 26)
              pushSTACK(TheCclosure(closure)->clos_consts[26]);
              goto next_byte;
            CASE cod_const_push27:           # (CONST&PUSH.S 27)
              pushSTACK(TheCclosure(closure)->clos_consts[27]);
              goto next_byte;
            CASE cod_const_push28:           # (CONST&PUSH.S 28)
              pushSTACK(TheCclosure(closure)->clos_consts[28]);
              goto next_byte;
            CASE cod_const_push29:           # (CONST&PUSH.S 29)
              pushSTACK(TheCclosure(closure)->clos_consts[29]);
              goto next_byte;
            #if 0
            CASE cod_const_push30:           # (CONST&PUSH.S 30)
              pushSTACK(TheCclosure(closure)->clos_consts[30]);
              goto next_byte;
            CASE cod_const_push31:           # (CONST&PUSH.S 31)
              pushSTACK(TheCclosure(closure)->clos_consts[31]);
              goto next_byte;
            CASE cod_const_push32:           # (CONST&PUSH.S 32)
              pushSTACK(TheCclosure(closure)->clos_consts[32]);
              goto next_byte;
            #endif
            CASE cod_store0:                 # (STORE.S 0)
              STACK_(0) = value1; mv_count=1;
              goto next_byte;
            CASE cod_store1:                 # (STORE.S 1)
              STACK_(1) = value1; mv_count=1;
              goto next_byte;
            CASE cod_store2:                 # (STORE.S 2)
              STACK_(2) = value1; mv_count=1;
              goto next_byte;
            CASE cod_store3:                 # (STORE.S 3)
              STACK_(3) = value1; mv_count=1;
              goto next_byte;
            CASE cod_store4:                 # (STORE.S 4)
              STACK_(4) = value1; mv_count=1;
              goto next_byte;
            CASE cod_store5:                 # (STORE.S 5)
              STACK_(5) = value1; mv_count=1;
              goto next_byte;
            CASE cod_store6:                 # (STORE.S 6)
              STACK_(6) = value1; mv_count=1;
              goto next_byte;
            CASE cod_store7:                 # (STORE.S 7)
              STACK_(7) = value1; mv_count=1;
              goto next_byte;
            #if 0
            CASE cod_store8:                 # (STORE.S 8)
              STACK_(8) = value1; mv_count=1;
              goto next_byte;
            CASE cod_store9:                 # (STORE.S 9)
              STACK_(9) = value1; mv_count=1;
              goto next_byte;
            CASE cod_store10:                # (STORE.S 10)
              STACK_(10) = value1; mv_count=1;
              goto next_byte;
            CASE cod_store11:                # (STORE.S 11)
              STACK_(11) = value1; mv_count=1;
              goto next_byte;
            CASE cod_store12:                # (STORE.S 12)
              STACK_(12) = value1; mv_count=1;
              goto next_byte;
            CASE cod_store13:                # (STORE.S 13)
              STACK_(13) = value1; mv_count=1;
              goto next_byte;
            CASE cod_store14:                # (STORE.S 14)
              STACK_(14) = value1; mv_count=1;
              goto next_byte;
            CASE cod_store15:                # (STORE.S 15)
              STACK_(15) = value1; mv_count=1;
              goto next_byte;
            CASE cod_store16:                # (STORE.S 16)
              STACK_(16) = value1; mv_count=1;
              goto next_byte;
            CASE cod_store17:                # (STORE.S 17)
              STACK_(17) = value1; mv_count=1;
              goto next_byte;
            CASE cod_store18:                # (STORE.S 18)
              STACK_(18) = value1; mv_count=1;
              goto next_byte;
            CASE cod_store19:                # (STORE.S 19)
              STACK_(19) = value1; mv_count=1;
              goto next_byte;
            CASE cod_store20:                # (STORE.S 20)
              STACK_(20) = value1; mv_count=1;
              goto next_byte;
            CASE cod_store21:                # (STORE.S 21)
              STACK_(21) = value1; mv_count=1;
              goto next_byte;
            #endif
            # ------------------- sonstiges -----------------------
            #ifndef FAST_DISPATCH
            default:
            #endif
              # undefinierter Code
              #if defined(GNU) && defined(FAST_SP)
                # -fomit-frame-pointer zunichte machen, damit
                # %sp bzw. %esp als private_SP verwendbar ist:
                alloca(1);
              #endif
              pushSTACK(fixnum(byteptr-&codeptr->data[0]-1)); # fehlerhafte Bytenummer
              pushSTACK(closure); # Closure
              fehler(serious_condition,
                     GETTEXT("undefined bytecode in ~ at byte ~")
                    );
            #undef L_operand
            #undef S_operand
            #undef U_operand
            #undef B_operand
            #undef CASE
          }
        fehler_zuviele_werte:
          fehler(error,
                 GETTEXT("too many return values")
                );
        #if STACKCHECKC
        fehler_STACK_putt:
          pushSTACK(fixnum(byteptr-&codeptr->data[0])); # PC
          pushSTACK(closure); # FUNC
          fehler(serious_condition,
                 GETTEXT("Corrupted STACK in ~ at byte ~")
                );
        #endif
        finished:
        #undef FREE_JMPBUF_on_SP
        #undef JMPBUF_on_SP
        #ifndef FAST_SP
        FREE_DYNAMIC_ARRAY(private_SP_space);
        #endif
        return;
    }}}


# wo ist check_SP() oder check_STACK() einzufügen??
# soll nest_env sein Ziel-Environment übergeben bekommen??
# Register-Allozierung in eval_subr und eval_cclosure usw.??
# subr_self eliminieren??

