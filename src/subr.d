# Liste aller SUBRs
# Bruno Haible 1990-1999

# Eine C-compilierte LISP-Funktion wird definiert durch eine Deklaration
#   LISPFUN(name,req_anz,opt_anz,rest_flag,key_flag,key_anz,keywords)
# in diesem File.
# > name: der Funktionsname (ein C-Identifier)
# > req_anz: die Anzahl der required-Parameter (eine Zahl)
# > opt_anz: die Anzahl der optional-Parameter (eine Zahl)
# > rest_flag: entweder norest oder rest
# > key_flag: entweder nokey oder key oder key_allow
# > key_anz: eine Zahl (0 falls nokey)
# > keywords: entweder NIL oder ein Ausdruck der Form (kw(keyword1),...,kw(keywordn))
#             (NIL falls nokey)

# Eine C-compilierte LISP-Funktion mit einer festen Anzahl Argumente
# wird definiert durch die akkürzende Deklaration
#   LISPFUNN(name,req_anz)
# > name: der Funktionsname (ein C-Identifier)
# > req_anz: die (feste) Anzahl der Argumente (eine Zahl)
  #define LISPFUNN(name,req_anz)  \
    LISPFUN(name,req_anz,0,norest,nokey,0,NIL)

# Zusätzlich muss in einem C-File dieselbe Deklaration samt C-Body stehen.


# Expander für die Konstruktion der extern-Deklarationen:
  #define LISPFUN_A(name,req_anz,opt_anz,rest_flag,key_flag,key_anz,keywords)  \
    extern subr_##rest_flag##_function C_##name;

# Expander für die Konstruktion der Deklaration der C-Funktion:
  #define LISPFUN_B(name,req_anz,opt_anz,rest_flag,key_flag,key_anz,keywords)  \
    global Values C_##name subr_##rest_flag##_function_args
  #define subr_norest_function_args  (void)
  #define subr_rest_function_args  (uintC argcount, object* rest_args_pointer)

# Expander für die Deklaration der SUBR-Tabelle:
  #define LISPFUN_C(name,req_anz,opt_anz,rest_flag,key_flag,key_anz,keywords)  \
    subr_ D_##name;

# Expander für die Initialisierung der SUBR-Tabelle:
  #define LISPFUN_D(name_,req_anz_,opt_anz_,rest_flag_,key_flag_,key_anz_,keywords_)  \
    ptr->function = (lisp_function)(&C_##name_);  \
    ptr->name = S_help_(S_##name_);               \
    ptr->keywords = NIL; # vorläufig              \
    ptr->argtype = (uintW)subr_argtype(req_anz_,opt_anz_,subr_##rest_flag_,subr_##key_flag_); \
    ptr->req_anz = req_anz_;                      \
    ptr->opt_anz = opt_anz_;                      \
    ptr->rest_flag = (uintB)subr_##rest_flag_;    \
    ptr->key_flag = (uintB)subr_##key_flag_;      \
    ptr->key_anz = key_anz_;                      \
    ptr++;
  #define LISPFUN_E(name_,req_anz,opt_anz,rest_flag,key_flag,key_anz,keywords)  \
    ptr->name = S_help_(S_##name_); \
    ptr++;
  #define LISPFUN_F(name,req_anz,opt_anz,rest_flag,key_flag,key_anz,keywords)  \
    { (lisp_function)(&C_##name), \
      nullobj, # vorläufig        \
      nullobj, # vorläufig        \
      0, # vorläufig              \
      req_anz,                    \
      opt_anz,                    \
      (uintB)subr_##rest_flag,    \
      (uintB)subr_##key_flag,     \
      key_anz,                    \
    },
  #define LISPFUN_G(name,req_anz,opt_anz,rest_flag,key_flag,key_anz,keywords)  \
    { (lisp_function)(&C_##name), \
      S_help_(S_##name),          \
      NIL, # vorläufig            \
      0, # vorläufig              \
      req_anz,                    \
      opt_anz,                    \
      (uintB)subr_##rest_flag,    \
      (uintB)subr_##key_flag,     \
      key_anz,                    \
    },

# Expander für die zweite Initialisierung der SUBR-Tabelle:
  #define LISPFUN_H(name,req_anz,opt_anz,rest_flag,key_flag,key_anz,keywords_)  \
    (subr_##key_flag==subr_key) ?            \
      subr_tab.D_##name.keywords =           \
        (vec = allocate_vector(key_anz),     \
         vecptr = &TheSvector(vec)->data[0], \
         (keywords_),                        \
         vec                                 \
        ) : 0;

# Welcher Expander benutzt wird, muss vom Hauptfile aus eingestellt werden.
# Default ist   #define LISPFUN LISPFUN_B


# ---------- SPVW ----------
# keine SUBRs
# ---------- EVAL ----------
LISPFUNN(funtabref,1)
LISPFUNN(subr_info,1)
# ---------- ARRAY ----------
LISPFUNN(copy_simple_vector,1)
LISPFUN(vector,0,0,rest,nokey,0,NIL)
LISPFUN(aref,1,0,rest,nokey,0,NIL)
LISPFUN(store,2,0,rest,nokey,0,NIL)
LISPFUNN(svref,2)
LISPFUNN(svstore,3)
LISPFUNN(psvstore,3)
LISPFUNN(row_major_aref,2)
LISPFUNN(row_major_store,3)
LISPFUNN(array_element_type,1)
LISPFUNN(array_rank,1)
LISPFUNN(array_dimension,2)
LISPFUNN(array_dimensions,1)
LISPFUNN(array_total_size,1)
LISPFUN(array_in_bounds_p,1,0,rest,nokey,0,NIL)
LISPFUN(array_row_major_index,1,0,rest,nokey,0,NIL)
LISPFUNN(adjustable_array_p,1)
LISPFUNN(array_displacement,1)
LISPFUN(bit,1,0,rest,nokey,0,NIL)
LISPFUN(sbit,1,0,rest,nokey,0,NIL)
LISPFUN(bit_and,2,1,norest,nokey,0,NIL)
LISPFUN(bit_ior,2,1,norest,nokey,0,NIL)
LISPFUN(bit_xor,2,1,norest,nokey,0,NIL)
LISPFUN(bit_eqv,2,1,norest,nokey,0,NIL)
LISPFUN(bit_nand,2,1,norest,nokey,0,NIL)
LISPFUN(bit_nor,2,1,norest,nokey,0,NIL)
LISPFUN(bit_andc1,2,1,norest,nokey,0,NIL)
LISPFUN(bit_andc2,2,1,norest,nokey,0,NIL)
LISPFUN(bit_orc1,2,1,norest,nokey,0,NIL)
LISPFUN(bit_orc2,2,1,norest,nokey,0,NIL)
LISPFUN(bit_not,1,1,norest,nokey,0,NIL)
LISPFUNN(array_has_fill_pointer_p,1)
LISPFUNN(fill_pointer,1)
LISPFUNN(set_fill_pointer,2)
LISPFUNN(vector_push,2)
LISPFUNN(vector_pop,1)
LISPFUN(vector_push_extend,2,1,norest,nokey,0,NIL)
LISPFUN(make_array,1,0,norest,key,7,
        (kw(adjustable),kw(element_type),kw(initial_element),
         kw(initial_contents),kw(fill_pointer),
         kw(displaced_to),kw(displaced_index_offset)) )
LISPFUN(adjust_array,2,0,norest,key,6,
        (kw(element_type),kw(initial_element),
         kw(initial_contents),kw(fill_pointer),
         kw(displaced_to),kw(displaced_index_offset)) )
LISPFUNN(vector_init,1)
LISPFUNN(vector_upd,2)
LISPFUNN(vector_endtest,2)
LISPFUNN(vector_fe_init,1)
LISPFUNN(vector_fe_upd,2)
LISPFUNN(vector_fe_endtest,2)
LISPFUNN(vector_length,1)
LISPFUNN(vector_init_start,2)
LISPFUNN(vector_fe_init_end,2)
LISPFUNN(make_bit_vector,1)
# ---------- CHARSTRG ----------
LISPFUNN(standard_char_p,1)
LISPFUNN(graphic_char_p,1)
LISPFUNN(string_char_p,1)
#if (base_char_code_limit < char_code_limit)
LISPFUNN(base_char_p,1)
#endif
LISPFUNN(alpha_char_p,1)
LISPFUNN(upper_case_p,1)
LISPFUNN(lower_case_p,1)
LISPFUNN(both_case_p,1)
LISPFUN(digit_char_p,1,1,norest,nokey,0,NIL)
LISPFUNN(alphanumericp,1)
LISPFUN(char_gleich,1,0,rest,nokey,0,NIL)
LISPFUN(char_ungleich,1,0,rest,nokey,0,NIL)
LISPFUN(char_kleiner,1,0,rest,nokey,0,NIL)
LISPFUN(char_groesser,1,0,rest,nokey,0,NIL)
LISPFUN(char_klgleich,1,0,rest,nokey,0,NIL)
LISPFUN(char_grgleich,1,0,rest,nokey,0,NIL)
LISPFUN(char_equal,1,0,rest,nokey,0,NIL)
LISPFUN(char_not_equal,1,0,rest,nokey,0,NIL)
LISPFUN(char_lessp,1,0,rest,nokey,0,NIL)
LISPFUN(char_greaterp,1,0,rest,nokey,0,NIL)
LISPFUN(char_not_greaterp,1,0,rest,nokey,0,NIL)
LISPFUN(char_not_lessp,1,0,rest,nokey,0,NIL)
LISPFUNN(char_code,1)
LISPFUNN(code_char,1)
LISPFUNN(character,1)
LISPFUNN(char_upcase,1)
LISPFUNN(char_downcase,1)
LISPFUN(digit_char,1,1,norest,nokey,0,NIL)
LISPFUNN(char_int,1)
LISPFUNN(int_char,1)
LISPFUNN(char_name,1)
LISPFUNN(char,2)
LISPFUNN(schar,2)
LISPFUNN(store_char,3)
LISPFUNN(store_schar,3)
LISPFUN(string_gleich,2,0,norest,key,4,
        (kw(start1),kw(end1),kw(start2),kw(end2)) )
LISPFUN(string_ungleich,2,0,norest,key,4,
        (kw(start1),kw(end1),kw(start2),kw(end2)) )
LISPFUN(string_kleiner,2,0,norest,key,4,
        (kw(start1),kw(end1),kw(start2),kw(end2)) )
LISPFUN(string_groesser,2,0,norest,key,4,
        (kw(start1),kw(end1),kw(start2),kw(end2)) )
LISPFUN(string_klgleich,2,0,norest,key,4,
        (kw(start1),kw(end1),kw(start2),kw(end2)) )
LISPFUN(string_grgleich,2,0,norest,key,4,
        (kw(start1),kw(end1),kw(start2),kw(end2)) )
LISPFUN(string_equal,2,0,norest,key,4,
        (kw(start1),kw(end1),kw(start2),kw(end2)) )
LISPFUN(string_not_equal,2,0,norest,key,4,
        (kw(start1),kw(end1),kw(start2),kw(end2)) )
LISPFUN(string_lessp,2,0,norest,key,4,
        (kw(start1),kw(end1),kw(start2),kw(end2)) )
LISPFUN(string_greaterp,2,0,norest,key,4,
        (kw(start1),kw(end1),kw(start2),kw(end2)) )
LISPFUN(string_not_greaterp,2,0,norest,key,4,
        (kw(start1),kw(end1),kw(start2),kw(end2)) )
LISPFUN(string_not_lessp,2,0,norest,key,4,
        (kw(start1),kw(end1),kw(start2),kw(end2)) )
LISPFUN(search_string_gleich,2,0,norest,key,4,
        (kw(start1),kw(end1),kw(start2),kw(end2)) )
LISPFUN(search_string_equal,2,0,norest,key,4,
        (kw(start1),kw(end1),kw(start2),kw(end2)) )
LISPFUN(make_string,1,0,norest,key,2, (kw(initial_element),kw(element_type)) )
LISPFUNN(string_both_trim,3)
LISPFUN(nstring_upcase,1,0,norest,key,2, (kw(start),kw(end)) )
LISPFUN(string_upcase,1,0,norest,key,2, (kw(start),kw(end)) )
LISPFUN(nstring_downcase,1,0,norest,key,2, (kw(start),kw(end)) )
LISPFUN(string_downcase,1,0,norest,key,2, (kw(start),kw(end)) )
LISPFUN(nstring_capitalize,1,0,norest,key,2, (kw(start),kw(end)) )
LISPFUN(string_capitalize,1,0,norest,key,2, (kw(start),kw(end)) )
LISPFUNN(string,1)
LISPFUNN(name_char,1)
LISPFUN(substring,2,1,norest,nokey,0,NIL)
LISPFUN(string_concat,0,0,rest,nokey,0,NIL)
# ---------- CONTROL ----------
LISPFUN(exit,0,1,norest,nokey,0,NIL)
LISPFUNN(psymbol_value,1)
LISPFUNN(symbol_value,1)
LISPFUNN(symbol_function,1)
LISPFUNN(fdefinition,1)
LISPFUNN(boundp,1)
LISPFUNN(fboundp,1)
LISPFUNN(special_operator_p,1)
LISPFUNN(set,2)
LISPFUNN(makunbound,1)
LISPFUNN(fmakunbound,1)
LISPFUN(apply,2,0,rest,nokey,0,NIL)
LISPFUN(pfuncall,1,0,rest,nokey,0,NIL)
LISPFUN(funcall,1,0,rest,nokey,0,NIL)
LISPFUN(mapcar,2,0,rest,nokey,0,NIL)
LISPFUN(maplist,2,0,rest,nokey,0,NIL)
LISPFUN(mapc,2,0,rest,nokey,0,NIL)
LISPFUN(mapl,2,0,rest,nokey,0,NIL)
LISPFUN(mapcan,2,0,rest,nokey,0,NIL)
LISPFUN(mapcon,2,0,rest,nokey,0,NIL)
LISPFUN(values,0,0,rest,nokey,0,NIL)
LISPFUNN(values_list,1)
LISPFUNN(driver,1)
LISPFUNN(unwind_to_driver,0)
LISPFUN(macro_function,1,1,norest,nokey,0,NIL)
LISPFUNN(old_macro_function,1)
LISPFUN(macroexpand,1,1,norest,nokey,0,NIL)
LISPFUN(macroexpand_1,1,1,norest,nokey,0,NIL)
LISPFUNN(proclaim,1)
LISPFUNN(eval,1)
LISPFUN(evalhook,3,1,norest,nokey,0,NIL)
LISPFUN(applyhook,4,1,norest,nokey,0,NIL)
LISPFUNN(constantp,1)
LISPFUNN(function_name_p,1)
LISPFUN(parse_body,1,2,norest,nokey,0,NIL)
LISPFUNN(keyword_test,2)
# ---------- DEBUG ----------
LISPFUN(read_form,1,1,norest,nokey,0,NIL)
LISPFUN(read_eval_print,1,1,norest,nokey,0,NIL)
LISPFUNN(load,1)
LISPFUNN(frame_up_1,2)
LISPFUNN(frame_up,2)
LISPFUNN(frame_down_1,2)
LISPFUNN(frame_down,2)
LISPFUNN(the_frame,0)
LISPFUNN(same_env_as,2)
LISPFUNN(eval_at,2)
LISPFUNN(eval_frame_p,1)
LISPFUNN(driver_frame_p,1)
LISPFUNN(trap_eval_frame,2)
LISPFUNN(redo_eval_frame,1)
LISPFUNN(return_from_eval_frame,2)
LISPFUNN(describe_frame,2)
LISPFUNN(show_stack,0)
LISPFUNN(debug,0)
LISPFUNN(proom,0)
LISPFUNN(gc,0)
# ---------- ENCODING ----------
LISPFUN(make_encoding,0,0,norest,key,4,
        (kw(charset),kw(line_terminator),kw(input_error_action),kw(output_error_action)) )
LISPFUNN(encodingp,1)
LISPFUNN(charset_typep,2)
LISPFUNN(charset_range,3)
LISPFUNN(default_file_encoding,0)
LISPFUNN(set_default_file_encoding,1)
#ifdef UNICODE
LISPFUNN(pathname_encoding,0)
LISPFUNN(set_pathname_encoding,1)
LISPFUNN(terminal_encoding,0)
LISPFUNN(set_terminal_encoding,1)
#if defined(HAVE_FFI) || defined(HAVE_AFFI)
LISPFUNN(foreign_encoding,0)
LISPFUNN(set_foreign_encoding,1)
#endif
LISPFUNN(misc_encoding,0)
LISPFUNN(set_misc_encoding,1)
#endif
# ---------- ERROR ----------
LISPFUN(error,1,0,rest,nokey,0,NIL)
LISPFUNN(defclcs,1)
LISPFUN(cerror_of_type,3,0,rest,nokey,0,NIL)
LISPFUN(error_of_type,2,0,rest,nokey,0,NIL)
LISPFUNN(invoke_debugger,1)
LISPFUN(clcs_signal,1,0,rest,nokey,0,NIL)
# ---------- HASHTABL ----------
LISPFUN(make_hash_table,0,0,norest,key,5,
        (kw(initial_contents),
         kw(test),kw(size),kw(rehash_size),kw(rehash_threshold)) )
LISPFUN(gethash,2,1,norest,nokey,0,NIL)
LISPFUNN(puthash,3)
LISPFUNN(remhash,2)
LISPFUNN(maphash,2)
LISPFUNN(clrhash,1)
LISPFUNN(hash_table_count,1)
LISPFUNN(hash_table_rehash_size,1)
LISPFUNN(hash_table_rehash_threshold,1)
LISPFUNN(hash_table_size,1)
LISPFUNN(hash_table_test,1)
LISPFUNN(hash_table_iterator,1)
LISPFUNN(hash_table_iterate,1)
LISPFUNN(class_gethash,2)
LISPFUN(class_tuple_gethash,2,0,rest,nokey,0,NIL)
LISPFUNN(sxhash,1)
# ---------- IO ----------
LISPFUNN(defio,2)
LISPFUN(copy_readtable,0,2,norest,nokey,0,NIL)
LISPFUN(set_syntax_from_char,2,2,norest,nokey,0,NIL)
LISPFUN(set_macro_character,2,2,norest,nokey,0,NIL)
LISPFUN(get_macro_character,1,1,norest,nokey,0,NIL)
LISPFUN(make_dispatch_macro_character,1,2,norest,nokey,0,NIL)
LISPFUN(set_dispatch_macro_character,3,1,norest,nokey,0,NIL)
LISPFUN(get_dispatch_macro_character,2,1,norest,nokey,0,NIL)
LISPFUNN(readtable_case,1)
LISPFUNN(set_readtable_case,2)
LISPFUNN(lpar_reader,2)
LISPFUNN(rpar_reader,2)
LISPFUNN(string_reader,2)
LISPFUNN(quote_reader,2)
LISPFUNN(line_comment_reader,2)
LISPFUNN(function_reader,3)
LISPFUNN(comment_reader,3)
LISPFUNN(char_reader,3)
LISPFUNN(binary_reader,3)
LISPFUNN(octal_reader,3)
LISPFUNN(hexadecimal_reader,3)
LISPFUNN(radix_reader,3)
LISPFUNN(complex_reader,3)
LISPFUNN(uninterned_reader,3)
LISPFUNN(bit_vector_reader,3)
LISPFUNN(vector_reader,3)
LISPFUNN(array_reader,3)
LISPFUNN(read_eval_reader,3)
LISPFUNN(load_eval_reader,3)
LISPFUNN(label_definition_reader,3)
LISPFUNN(label_reference_reader,3)
LISPFUNN(not_readable_reader,3)
LISPFUNN(syntax_error_reader,3)
LISPFUNN(feature_reader,3)
LISPFUNN(not_feature_reader,3)
LISPFUNN(structure_reader,3)
LISPFUNN(closure_reader,3)
LISPFUNN(clisp_pathname_reader,3)
LISPFUNN(ansi_pathname_reader,3)
#ifdef UNIX
LISPFUNN(unix_executable_reader,3)
#endif
LISPFUN(read,0,4,norest,nokey,0,NIL)
LISPFUN(read_preserving_whitespace,0,4,norest,nokey,0,NIL)
LISPFUN(read_delimited_list,1,2,norest,nokey,0,NIL)
LISPFUN(read_line,0,4,norest,nokey,0,NIL)
LISPFUN(read_char,0,4,norest,nokey,0,NIL)
LISPFUN(unread_char,1,1,norest,nokey,0,NIL)
LISPFUN(peek_char,0,5,norest,nokey,0,NIL)
LISPFUN(listen,0,1,norest,nokey,0,NIL)
LISPFUNN(read_char_will_hang_p,1)
LISPFUN(read_char_no_hang,0,4,norest,nokey,0,NIL)
LISPFUN(clear_input,0,1,norest,nokey,0,NIL)
LISPFUN(read_from_string,1,2,norest,key,3,
        (kw(preserve_whitespace),kw(start),kw(end)) )
LISPFUN(parse_integer,1,0,norest,key,4,
        (kw(start),kw(end),kw(radix),kw(junk_allowed)) )
LISPFUNN(print_structure,2)
LISPFUN(write,1,0,norest,key,14,
        (kw(case),kw(level),kw(length),kw(gensym),kw(escape),kw(radix),
         kw(base),kw(array),kw(circle),kw(pretty),kw(closure),kw(readably),
         kw(right_margin),kw(stream)))
LISPFUN(prin1,1,1,norest,nokey,0,NIL)
LISPFUN(print,1,1,norest,nokey,0,NIL)
LISPFUN(pprint,1,1,norest,nokey,0,NIL)
LISPFUN(princ,1,1,norest,nokey,0,NIL)
LISPFUN(write_to_string,1,0,norest,key,13,
        (kw(case),kw(level),kw(length),kw(gensym),kw(escape),kw(radix),
         kw(base),kw(array),kw(circle),kw(pretty),kw(closure),kw(readably),
         kw(right_margin)))
LISPFUNN(prin1_to_string,1)
LISPFUNN(princ_to_string,1)
LISPFUN(write_char,1,1,norest,nokey,0,NIL)
LISPFUN(write_string,1,1,norest,key,2, (kw(start),kw(end)) )
LISPFUN(write_line,1,1,norest,key,2, (kw(start),kw(end)) )
LISPFUN(terpri,0,1,norest,nokey,0,NIL)
LISPFUN(fresh_line,0,1,norest,nokey,0,NIL)
LISPFUN(finish_output,0,1,norest,nokey,0,NIL)
LISPFUN(force_output,0,1,norest,nokey,0,NIL)
LISPFUN(clear_output,0,1,norest,nokey,0,NIL)
LISPFUN(write_unreadable,3,0,norest,key,2, (kw(type),kw(identity)) )
LISPFUN(line_position,0,1,norest,nokey,0,NIL)
# ---------- LIST ----------
LISPFUNN(car,1)
LISPFUNN(cdr,1)
LISPFUNN(caar,1)
LISPFUNN(cadr,1)
LISPFUNN(cdar,1)
LISPFUNN(cddr,1)
LISPFUNN(caaar,1)
LISPFUNN(caadr,1)
LISPFUNN(cadar,1)
LISPFUNN(caddr,1)
LISPFUNN(cdaar,1)
LISPFUNN(cdadr,1)
LISPFUNN(cddar,1)
LISPFUNN(cdddr,1)
LISPFUNN(caaaar,1)
LISPFUNN(caaadr,1)
LISPFUNN(caadar,1)
LISPFUNN(caaddr,1)
LISPFUNN(cadaar,1)
LISPFUNN(cadadr,1)
LISPFUNN(caddar,1)
LISPFUNN(cadddr,1)
LISPFUNN(cdaaar,1)
LISPFUNN(cdaadr,1)
LISPFUNN(cdadar,1)
LISPFUNN(cdaddr,1)
LISPFUNN(cddaar,1)
LISPFUNN(cddadr,1)
LISPFUNN(cdddar,1)
LISPFUNN(cddddr,1)
LISPFUNN(cons,2)
LISPFUN(tree_equal,2,0,norest,key,2, (kw(test),kw(test_not)) )
LISPFUNN(endp,1)
LISPFUNN(list_length,1)
LISPFUNN(nth,2)
LISPFUNN(first,1)
LISPFUNN(second,1)
LISPFUNN(third,1)
LISPFUNN(fourth,1)
LISPFUNN(fifth,1)
LISPFUNN(sixth,1)
LISPFUNN(seventh,1)
LISPFUNN(eighth,1)
LISPFUNN(ninth,1)
LISPFUNN(tenth,1)
LISPFUNN(rest,1)
LISPFUNN(nthcdr,2)
LISPFUN(last,1,1,norest,nokey,0,NIL)
LISPFUN(list,0,0,rest,nokey,0,NIL)
LISPFUN(liststern,1,0,rest,nokey,0,NIL)
LISPFUN(make_list,1,0,norest,key,1, (kw(initial_element)) )
LISPFUN(append,0,0,rest,nokey,0,NIL)
LISPFUNN(copy_list,1)
LISPFUNN(copy_alist,1)
LISPFUNN(copy_tree,1)
LISPFUNN(revappend,2)
LISPFUN(nconc,0,0,rest,nokey,0,NIL)
LISPFUNN(nreconc,2)
LISPFUNN(list_nreverse,1)
LISPFUN(butlast,1,1,norest,nokey,0,NIL)
LISPFUN(nbutlast,1,1,norest,nokey,0,NIL)
LISPFUNN(ldiff,2)
LISPFUNN(rplaca,2)
LISPFUNN(prplaca,2)
LISPFUNN(rplacd,2)
LISPFUNN(prplacd,2)
LISPFUN(subst,3,0,norest,key,3, (kw(test),kw(test_not),kw(key)) )
LISPFUN(subst_if,3,0,norest,key,1, (kw(key)) )
LISPFUN(subst_if_not,3,0,norest,key,1, (kw(key)) )
LISPFUN(nsubst,3,0,norest,key,3, (kw(test),kw(test_not),kw(key)) )
LISPFUN(nsubst_if,3,0,norest,key,1, (kw(key)) )
LISPFUN(nsubst_if_not,3,0,norest,key,1, (kw(key)) )
LISPFUN(sublis,2,0,norest,key,3, (kw(test),kw(test_not),kw(key)) )
LISPFUN(nsublis,2,0,norest,key,3, (kw(test),kw(test_not),kw(key)) )
LISPFUN(member,2,0,norest,key,3, (kw(test),kw(test_not),kw(key)) )
LISPFUN(member_if,2,0,norest,key,1, (kw(key)) )
LISPFUN(member_if_not,2,0,norest,key,1, (kw(key)) )
LISPFUNN(tailp,2)
LISPFUN(adjoin,2,0,norest,key,3, (kw(test),kw(test_not),kw(key)) )
LISPFUNN(acons,3)
LISPFUN(pairlis,2,1,norest,nokey,0,NIL)
LISPFUN(assoc,2,0,norest,key,3, (kw(test),kw(test_not),kw(key)) )
LISPFUN(assoc_if,2,0,norest,key,1, (kw(key)) )
LISPFUN(assoc_if_not,2,0,norest,key,1, (kw(key)) )
LISPFUN(rassoc,2,0,norest,key,3, (kw(test),kw(test_not),kw(key)) )
LISPFUN(rassoc_if,2,0,norest,key,1, (kw(key)) )
LISPFUN(rassoc_if_not,2,0,norest,key,1, (kw(key)) )
LISPFUNN(list_upd,2)
LISPFUNN(list_endtest,2)
LISPFUNN(list_fe_init,1)
LISPFUNN(list_access,2)
LISPFUNN(list_access_set,3)
LISPFUNN(list_llength,1)
LISPFUNN(list_elt,2)
LISPFUNN(list_set_elt,3)
LISPFUNN(list_init_start,2)
LISPFUNN(list_fe_init_end,2)
# ---------- MISC ----------
LISPFUNN(lisp_implementation_type,0)
LISPFUNN(lisp_implementation_version,0)
LISPFUN(version,0,1,norest,nokey,0,NIL)
#ifdef MACHINE_KNOWN
LISPFUNN(machinetype,0)
LISPFUNN(machine_version,0)
#endif
#ifdef HAVE_ENVIRONMENT
LISPFUNN(get_env,1)
#endif
#ifdef WIN32_NATIVE
LISPFUNN(registry,2)
#endif
LISPFUNN(software_type,0)
LISPFUNN(software_version,0)
LISPFUNN(current_language,0)
LISPFUNN(language,3)
LISPFUNN(identity,1)
LISPFUNN(address_of,1)
#ifdef HAVE_DISASSEMBLER
LISPFUNN(code_address_of,1)
LISPFUNN(program_id,0)
#endif
LISPFUNN(ansi,0)
LISPFUNN(set_ansi,1)
# ---------- SOCKET ----------
#ifdef MACHINE_KNOWN
LISPFUNN(machine_instance,0)
#endif
#ifdef HAVE_GETHOSTBYNAME
LISPFUN(socket_service_port,0,2,norest,nokey,0,NIL)
#ifdef EXPORT_SYSCALLS
LISPFUN(resolve_host_ipaddr,0,1,norest,nokey,0,NIL)
#endif
#endif
# ---------- TIME ----------
LISPFUNN(get_universal_time,0)
#if defined(UNIX) || defined(WIN32)
LISPFUN(default_time_zone,0,1,norest,nokey,0,NIL)
#endif
LISPFUNN(get_internal_run_time,0)
LISPFUNN(get_internal_real_time,0)
#ifdef SLEEP_1
LISPFUNN(sleep,1)
#endif
#ifdef SLEEP_2
LISPFUNN(sleep,2)
#endif
LISPFUNN(time,0)
# ---------- PACKAGE ----------
LISPFUNN(make_symbol,1)
LISPFUNN(find_package,1)
LISPFUNN(pfind_package,1)
LISPFUNN(package_name,1)
LISPFUNN(package_nicknames,1)
LISPFUN(rename_package,2,1,norest,nokey,0,NIL)
LISPFUNN(package_use_list,1)
LISPFUNN(package_used_by_list,1)
LISPFUNN(package_shadowing_symbols,1)
LISPFUNN(list_all_packages,0)
LISPFUN(intern,1,1,norest,nokey,0,NIL)
LISPFUN(find_symbol,1,1,norest,nokey,0,NIL)
LISPFUN(unintern,1,1,norest,nokey,0,NIL)
LISPFUN(export,1,1,norest,nokey,0,NIL)
LISPFUN(unexport,1,1,norest,nokey,0,NIL)
LISPFUN(import,1,1,norest,nokey,0,NIL)
LISPFUN(shadowing_import,1,1,norest,nokey,0,NIL)
LISPFUN(shadow,1,1,norest,nokey,0,NIL)
LISPFUN(use_package,1,1,norest,nokey,0,NIL)
LISPFUN(unuse_package,1,1,norest,nokey,0,NIL)
LISPFUN(make_package,1,0,norest,key,3, (kw(nicknames),kw(use),kw(case_sensitive)) )
LISPFUN(pin_package,1,0,norest,key,3, (kw(nicknames),kw(use),kw(case_sensitive)) )
LISPFUN(in_package,1,0,norest,key,3, (kw(nicknames),kw(use),kw(case_sensitive)) )
LISPFUNN(delete_package,1)
LISPFUNN(find_all_symbols,1)
LISPFUNN(map_symbols,2)
LISPFUNN(map_external_symbols,2)
LISPFUNN(map_all_symbols,1)
LISPFUNN(package_iterator,2)
LISPFUNN(package_iterate,1)
# ---------- PATHNAME ----------
LISPFUN(parse_namestring,1,2,norest,key,3,
        (kw(start),kw(end),kw(junk_allowed)) )
LISPFUNN(pathname,1)
LISPFUN(pathnamehost,1,0,norest,key,1, (kw(case)))
LISPFUN(pathnamedevice,1,0,norest,key,1, (kw(case)))
LISPFUN(pathnamedirectory,1,0,norest,key,1, (kw(case)))
LISPFUN(pathnamename,1,0,norest,key,1, (kw(case)))
LISPFUN(pathnametype,1,0,norest,key,1, (kw(case)))
LISPFUNN(pathnameversion,1)
#ifdef LOGICAL_PATHNAMES
LISPFUNN(logical_pathname,1)
LISPFUN(translate_logical_pathname,1,0,norest,key,0,_EMA_)
#endif
LISPFUNN(file_namestring,1)
LISPFUNN(directory_namestring,1)
LISPFUNN(host_namestring,1)
LISPFUN(merge_pathnames,1,2,norest,key,1, (kw(wild)))
LISPFUN(enough_namestring,1,1,norest,nokey,0,NIL)
LISPFUN(make_pathname,0,0,norest,key,8,
        (kw(defaults),kw(case),kw(host),kw(device),kw(directory),kw(name),kw(type),kw(version)) )
#ifdef LOGICAL_PATHNAMES
LISPFUN(make_logical_pathname,0,0,norest,key,8,
        (kw(defaults),kw(case),kw(host),kw(device),kw(directory),kw(name),kw(type),kw(version)) )
#endif
#ifdef USER_HOMEDIR
LISPFUN(user_homedir_pathname,0,1,norest,nokey,0,NIL)
#endif
LISPFUN(wild_pathname_p,1,1,norest,nokey,0,NIL)
LISPFUNN(pathname_match_p,2)
LISPFUN(translate_pathname,3,0,norest,key,2, (kw(all),kw(merge)))
LISPFUN(namestring,1,1,norest,nokey,0,NIL)
LISPFUNN(truename,1)
LISPFUNN(probe_file,1)
LISPFUNN(probe_directory,1)
LISPFUNN(delete_file,1)
LISPFUNN(rename_file,2)
LISPFUN(old_open,1,0,norest,key,5,
        (kw(direction),kw(element_type),kw(if_exists),kw(if_does_not_exist),kw(external_format)) )
LISPFUN(open,1,0,norest,key,6,
        (kw(direction),kw(element_type),kw(if_exists),kw(if_does_not_exist),kw(external_format),kw(buffered)) )
LISPFUN(directory,0,1,norest,key,2, (kw(circle),kw(full)) )
LISPFUN(cd,0,1,norest,nokey,0,NIL)
LISPFUNN(make_dir,1)
LISPFUNN(delete_dir,1)
LISPFUN(ensure_directories_exist,1,0,norest,key,1,(kw(verbose)))
LISPFUNN(file_write_date,1)
LISPFUNN(file_author,1)
#if defined(UNIX) || defined(MSDOS) || defined(RISCOS)
LISPFUN(execute,1,0,rest,nokey,0,NIL)
#endif
#if defined(AMIGAOS)
LISPFUN(execute,1,0,norest,nokey,0,NIL)
#endif
#ifdef HAVE_SHELL
#ifdef WIN32_NATIVE
LISPFUNN(shell_name,0)
#endif
LISPFUN(shell,0,1,norest,nokey,0,NIL)
#endif
LISPFUNN(savemem,1)
#ifdef DYNAMIC_MODULES
LISPFUNN(dynload_modules,2)
#endif
#ifdef HAVE_DISASSEMBLER
LISPFUNN(program_name,0)
#endif
LISPFUNN(lib_directory,0)
#ifdef EXPORT_SYSCALLS
#ifdef UNIX
LISPFUN(user_data,0,1,norest,nokey,0,NIL)
LISPFUN(file_stat,1,1,norest,nokey,0,NIL)
#endif
#endif
# ---------- PREDTYPE ----------
LISPFUNN(eq,2)
LISPFUNN(eql,2)
LISPFUNN(equal,2)
LISPFUNN(equalp,2)
LISPFUNN(consp,1)
LISPFUNN(atom,1)
LISPFUNN(symbolp,1)
LISPFUNN(stringp,1)
LISPFUNN(numberp,1)
LISPFUNN(compiled_function_p,1)
LISPFUNN(null,1)
LISPFUNN(not,1)
LISPFUNN(closurep,1)
LISPFUNN(listp,1)
LISPFUNN(integerp,1)
LISPFUNN(fixnump,1)
LISPFUNN(rationalp,1)
LISPFUNN(floatp,1)
LISPFUNN(short_float_p,1)
LISPFUNN(single_float_p,1)
LISPFUNN(double_float_p,1)
LISPFUNN(long_float_p,1)
LISPFUNN(realp,1)
LISPFUNN(complexp,1)
LISPFUNN(streamp,1)
LISPFUNN(built_in_stream_p,1)
LISPFUNN(random_state_p,1)
LISPFUNN(readtablep,1)
LISPFUNN(hash_table_p,1)
LISPFUNN(pathnamep,1)
LISPFUNN(logical_pathname_p,1)
LISPFUNN(characterp,1)
LISPFUNN(functionp,1)
LISPFUNN(generic_function_p,1)
LISPFUNN(packagep,1)
LISPFUNN(arrayp,1)
LISPFUNN(simple_array_p,1)
LISPFUNN(bit_vector_p,1)
LISPFUNN(vectorp,1)
LISPFUNN(simple_vector_p,1)
LISPFUNN(simple_string_p,1)
LISPFUNN(simple_bit_vector_p,1)
LISPFUNN(commonp,1)
LISPFUNN(type_of,1)
LISPFUNN(defclos,2)
LISPFUNN(class_p,1)
LISPFUNN(class_of,1)
LISPFUN(find_class,1,2,norest,nokey,0,NIL)
LISPFUNN(coerce,2)
LISPFUNN(note_new_structure_class,0)
LISPFUNN(note_new_standard_class,0)
LISPFUNN(heap_statistics,0)
LISPFUNN(gc_statistics,0)
LISPFUNN(list_statistics,1)
LISPFUNN(heap_statistics_statistics,1)
LISPFUNN(gc_statistics_statistics,2)
# ---------- RECORD ----------
LISPFUNN(record_ref,2)
LISPFUNN(record_store,3)
LISPFUNN(record_length,1)
LISPFUNN(pstructure_ref,3)
LISPFUNN(structure_ref,3)
LISPFUNN(structure_store,4)
LISPFUNN(make_structure,2)
LISPFUNN(copy_structure,1)
LISPFUNN(structure_type_p,2)
LISPFUNN(closure_name,1)
LISPFUNN(closure_codevec,1)
LISPFUNN(closure_consts,1)
LISPFUNN(make_code_vector,1)
LISPFUNN(make_closure,3)
LISPFUNN(copy_generic_function,2)
LISPFUNN(generic_function_effective_method_function,1)
LISPFUNN(make_load_time_eval,1)
LISPFUNN(make_symbol_macro,1)
LISPFUNN(symbol_macro_p,1)
LISPFUNN(symbol_macro_expand,1)
LISPFUNN(make_weak_pointer,1)
LISPFUNN(weak_pointer_p,1)
LISPFUNN(weak_pointer_value,1)
LISPFUN(finalize,2,1,norest,nokey,0,NIL)
LISPFUNN(structure_object_p,1)
LISPFUNN(std_instance_p,1)
LISPFUNN(allocate_std_instance,2)
LISPFUNN(old_pallocate_instance,1)
LISPFUN(pallocate_instance,1,0,rest,nokey,0,NIL)
LISPFUNN(slot_value,2)
LISPFUNN(set_slot_value,3)
LISPFUNN(slot_boundp,2)
LISPFUNN(slot_makunbound,2)
LISPFUNN(slot_exists_p,2)
LISPFUN(shared_initialize,2,0,rest,nokey,0,NIL)
LISPFUN(reinitialize_instance,1,0,rest,nokey,0,NIL)
LISPFUN(initialize_instance,1,0,rest,nokey,0,NIL)
LISPFUN(make_instance,1,0,rest,nokey,0,NIL)
# ---------- SEQUENCE ----------
LISPFUNN(sequencep,1)
LISPFUNN(defseq,1)
LISPFUNN(elt,2)
LISPFUNN(setelt,3)
LISPFUN(subseq,2,1,norest,nokey,0,NIL)
LISPFUNN(copy_seq,1)
LISPFUNN(length,1)
LISPFUNN(reverse,1)
LISPFUNN(nreverse,1)
LISPFUN(make_sequence,2,0,norest,key,2,
        (kw(initial_element),kw(update)) )
LISPFUN(concatenate,1,0,rest,nokey,0,NIL)
LISPFUN(map,3,0,rest,nokey,0,NIL)
LISPFUN(map_into,2,0,rest,nokey,0,NIL)
LISPFUN(some,2,0,rest,nokey,0,NIL)
LISPFUN(every,2,0,rest,nokey,0,NIL)
LISPFUN(notany,2,0,rest,nokey,0,NIL)
LISPFUN(notevery,2,0,rest,nokey,0,NIL)
LISPFUN(reduce,2,0,norest,key,5,
        (kw(from_end),kw(start),kw(end),kw(key),kw(initial_value)) )
LISPFUN(fill,2,0,norest,key,2, (kw(start),kw(end)) )
LISPFUN(replace,2,0,norest,key,4,
        (kw(start1),kw(end1),kw(start2),kw(end2)) )
LISPFUN(remove,2,0,norest,key,7,
        (kw(from_end),kw(start),kw(end),kw(key),kw(test),kw(test_not),kw(count)) )
LISPFUN(remove_if,2,0,norest,key,5,
        (kw(from_end),kw(start),kw(end),kw(key),kw(count)) )
LISPFUN(remove_if_not,2,0,norest,key,5,
        (kw(from_end),kw(start),kw(end),kw(key),kw(count)) )
LISPFUN(delete,2,0,norest,key,7,
        (kw(from_end),kw(start),kw(end),kw(key),kw(test),kw(test_not),kw(count)) )
LISPFUN(delete_if,2,0,norest,key,5,
        (kw(from_end),kw(start),kw(end),kw(key),kw(count)) )
LISPFUN(delete_if_not,2,0,norest,key,5,
        (kw(from_end),kw(start),kw(end),kw(key),kw(count)) )
LISPFUN(remove_duplicates,1,0,norest,key,6,
        (kw(from_end),kw(start),kw(end),kw(key),kw(test),kw(test_not)) )
LISPFUN(delete_duplicates,1,0,norest,key,6,
        (kw(from_end),kw(start),kw(end),kw(key),kw(test),kw(test_not)) )
LISPFUN(substitute,3,0,norest,key,7,
        (kw(from_end),kw(start),kw(end),kw(key),kw(test),kw(test_not),kw(count)) )
LISPFUN(substitute_if,3,0,norest,key,5,
        (kw(from_end),kw(start),kw(end),kw(key),kw(count)) )
LISPFUN(substitute_if_not,3,0,norest,key,5,
        (kw(from_end),kw(start),kw(end),kw(key),kw(count)) )
LISPFUN(nsubstitute,3,0,norest,key,7,
        (kw(from_end),kw(start),kw(end),kw(key),kw(test),kw(test_not),kw(count)) )
LISPFUN(nsubstitute_if,3,0,norest,key,5,
        (kw(from_end),kw(start),kw(end),kw(key),kw(count)) )
LISPFUN(nsubstitute_if_not,3,0,norest,key,5,
        (kw(from_end),kw(start),kw(end),kw(key),kw(count)) )
LISPFUN(find,2,0,norest,key,6,
        (kw(from_end),kw(start),kw(end),kw(key),kw(test),kw(test_not)) )
LISPFUN(find_if,2,0,norest,key,4,
        (kw(from_end),kw(start),kw(end),kw(key)) )
LISPFUN(find_if_not,2,0,norest,key,4,
        (kw(from_end),kw(start),kw(end),kw(key)) )
LISPFUN(position,2,0,norest,key,6,
        (kw(from_end),kw(start),kw(end),kw(key),kw(test),kw(test_not)) )
LISPFUN(position_if,2,0,norest,key,4,
        (kw(from_end),kw(start),kw(end),kw(key)) )
LISPFUN(position_if_not,2,0,norest,key,4,
        (kw(from_end),kw(start),kw(end),kw(key)) )
LISPFUN(count,2,0,norest,key,6,
        (kw(from_end),kw(start),kw(end),kw(key),kw(test),kw(test_not)) )
LISPFUN(count_if,2,0,norest,key,4,
        (kw(from_end),kw(start),kw(end),kw(key)) )
LISPFUN(count_if_not,2,0,norest,key,4,
        (kw(from_end),kw(start),kw(end),kw(key)) )
LISPFUN(mismatch,2,0,norest,key,8,
        (kw(start1),kw(end1),kw(start2),kw(end2),kw(from_end),
         kw(key),kw(test),kw(test_not)) )
LISPFUN(search,2,0,norest,key,8,
        (kw(start1),kw(end1),kw(start2),kw(end2),kw(from_end),
         kw(key),kw(test),kw(test_not)) )
LISPFUN(sort,2,0,norest,key,3, (kw(key),kw(start),kw(end)) )
LISPFUN(stable_sort,2,0,norest,key,3, (kw(key),kw(start),kw(end)) )
LISPFUN(merge,4,0,norest,key,1, (kw(key)) )
LISPFUN(read_char_sequence,2,0,norest,key,2, (kw(start),kw(end)) )
LISPFUN(write_char_sequence,2,0,norest,key,2, (kw(start),kw(end)) )
LISPFUN(read_byte_sequence,2,0,norest,key,2, (kw(start),kw(end)) )
LISPFUN(write_byte_sequence,2,0,norest,key,2, (kw(start),kw(end)) )
# ---------- STREAM ----------
LISPFUN(symbol_stream,1,1,norest,nokey,0,NIL)
LISPFUNN(make_synonym_stream,1)
LISPFUNN(synonym_stream_p,1)
LISPFUNN(synonym_stream_symbol,1)
LISPFUN(make_broadcast_stream,0,0,rest,nokey,0,NIL)
LISPFUNN(broadcast_stream_p,1)
LISPFUNN(broadcast_stream_streams,1)
LISPFUN(make_concatenated_stream,0,0,rest,nokey,0,NIL)
LISPFUNN(concatenated_stream_p,1)
LISPFUNN(concatenated_stream_streams,1)
LISPFUNN(make_two_way_stream,2)
LISPFUNN(two_way_stream_p,1)
LISPFUNN(two_way_stream_input_stream,1)
LISPFUNN(two_way_stream_output_stream,1)
LISPFUNN(make_echo_stream,2)
LISPFUNN(echo_stream_p,1)
LISPFUNN(echo_stream_input_stream,1)
LISPFUNN(echo_stream_output_stream,1)
LISPFUN(make_string_input_stream,1,2,norest,nokey,0,NIL)
LISPFUNN(string_input_stream_index,1)
LISPFUN(make_string_output_stream,0,0,norest,key,2, (kw(element_type),kw(line_position)))
LISPFUNN(get_output_stream_string,1)
LISPFUNN(make_string_push_stream,1)
LISPFUNN(string_stream_p,1)
LISPFUNN(make_buffered_input_stream,2)
LISPFUNN(buffered_input_stream_index,1)
LISPFUN(make_buffered_output_stream,1,1,norest,nokey,0,NIL)
#ifdef GENERIC_STREAMS
LISPFUNN(generic_stream_controller,1)
LISPFUNN(make_generic_stream,1)
LISPFUNN(generic_stream_p,1)
#endif
#ifdef KEYBOARD
LISPFUNN(make_keyboard_stream,0)
#endif
LISPFUN(terminal_raw,2,1,norest,nokey,0,NIL)
#ifdef SCREEN
LISPFUNN(make_window,0)
LISPFUNN(window_size,1)
LISPFUNN(window_cursor_position,1)
LISPFUNN(set_window_cursor_position,3)
LISPFUNN(clear_window,1)
LISPFUNN(clear_window_to_eot,1)
LISPFUNN(clear_window_to_eol,1)
LISPFUNN(delete_window_line,1)
LISPFUNN(insert_window_line,1)
LISPFUNN(highlight_on,1)
LISPFUNN(highlight_off,1)
LISPFUNN(window_cursor_on,1)
LISPFUNN(window_cursor_off,1)
#endif
LISPFUNN(file_stream_p,1)
#ifdef PRINTER_AMIGAOS
LISPFUNN(make_printer_stream,0)
#endif
#ifdef PIPES
LISPFUN(make_pipe_input_stream,1,0,norest,key,3,\
        (kw(element_type),kw(external_format),kw(buffered)) )
LISPFUN(make_pipe_output_stream,1,0,norest,key,3,\
        (kw(element_type),kw(external_format),kw(buffered)) )
#ifdef PIPES2
LISPFUN(make_pipe_io_stream,1,0,norest,key,3,\
        (kw(element_type),kw(external_format),kw(buffered)) )
#endif
#endif
#ifdef X11SOCKETS
LISPFUNN(make_x11socket_stream,2)
LISPFUNN(listen_byte,1)
LISPFUNN(read_n_bytes,4)
LISPFUNN(write_n_bytes,4)
#endif
#ifdef SOCKET_STREAMS
LISPFUNN(socket_server_close,1)
LISPFUN(socket_server,0,1,norest,nokey,0,NIL)
LISPFUNN(socket_server_port,1)
LISPFUNN(socket_server_host,1)
LISPFUN(socket_accept,1,0,norest,key,3,\
        (kw(element_type),kw(external_format),kw(buffered)) )
LISPFUN(socket_wait,1,2,norest,nokey,0,NIL)
LISPFUN(socket_connect,1,1,norest,key,3,\
        (kw(element_type),kw(external_format),kw(buffered)) )
LISPFUNN(socket_stream_port,1)
LISPFUNN(socket_stream_host,1)
LISPFUNN(socket_stream_peer,1)
LISPFUNN(socket_stream_local,1)
#ifndef WIN32_NATIVE
LISPFUNN(socket_stream_handle,1)
#endif
#endif
LISPFUNN(built_in_stream_open_p,1)
LISPFUNN(input_stream_p,1)
LISPFUNN(output_stream_p,1)
LISPFUNN(built_in_stream_element_type,1)
LISPFUNN(built_in_stream_set_element_type,2)
LISPFUNN(stream_external_format,1)
LISPFUN(set_stream_external_format,2,1,norest,nokey,0,NIL)
LISPFUNN(interactive_stream_p,1)
LISPFUN(built_in_stream_close,1,0,norest,key,1, (kw(abort)) )
LISPFUN(read_byte,1,2,norest,nokey,0,NIL)
LISPFUN(read_integer,2,3,norest,nokey,0,NIL)
LISPFUNN(write_byte,2)
LISPFUN(write_integer,3,1,norest,nokey,0,NIL)
LISPFUN(file_position,1,1,norest,nokey,0,NIL)
LISPFUNN(file_length,1)
LISPFUNN(file_string_length,2)
LISPFUNN(line_number,1)
LISPFUN(allow_read_eval,1,1,norest,nokey,0,NIL)
LISPFUNN(defgray,1)
# ---------- SYMBOL ----------
LISPFUNN(putd,2)
LISPFUNN(find_subr,1)
LISPFUNN(proclaim_constant,2)
LISPFUN(get,2,1,norest,nokey,0,NIL)
LISPFUN(getf,2,1,norest,nokey,0,NIL)
LISPFUNN(get_properties,2)
LISPFUNN(putplist,2)
LISPFUNN(put,3)
LISPFUNN(remprop,2)
LISPFUNN(symbol_package,1)
LISPFUNN(symbol_plist,1)
LISPFUNN(symbol_name,1)
LISPFUNN(keywordp,1)
LISPFUNN(special_variable_p,1)
LISPFUN(gensym,0,1,norest,nokey,0,NIL)
# ---------- LISPARIT ----------
LISPFUNN(decimal_string,1)
LISPFUNN(zerop,1)
LISPFUNN(plusp,1)
LISPFUNN(minusp,1)
LISPFUNN(oddp,1)
LISPFUNN(evenp,1)
LISPFUN(gleich,1,0,rest,nokey,0,NIL)
LISPFUN(ungleich,1,0,rest,nokey,0,NIL)
LISPFUN(kleiner,1,0,rest,nokey,0,NIL)
LISPFUN(groesser,1,0,rest,nokey,0,NIL)
LISPFUN(klgleich,1,0,rest,nokey,0,NIL)
LISPFUN(grgleich,1,0,rest,nokey,0,NIL)
LISPFUN(max,1,0,rest,nokey,0,NIL)
LISPFUN(min,1,0,rest,nokey,0,NIL)
LISPFUN(plus,0,0,rest,nokey,0,NIL)
LISPFUN(minus,1,0,rest,nokey,0,NIL)
LISPFUN(mal,0,0,rest,nokey,0,NIL)
LISPFUN(durch,1,0,rest,nokey,0,NIL)
LISPFUNN(einsplus,1)
LISPFUNN(einsminus,1)
LISPFUNN(conjugate,1)
LISPFUN(gcd,0,0,rest,nokey,0,NIL)
LISPFUN(xgcd,0,0,rest,nokey,0,NIL)
LISPFUN(lcm,0,0,rest,nokey,0,NIL)
LISPFUNN(exp,1)
LISPFUNN(expt,2)
LISPFUN(log,1,1,norest,nokey,0,NIL)
LISPFUNN(sqrt,1)
LISPFUNN(isqrt,1)
LISPFUNN(abs,1)
LISPFUNN(phase,1)
LISPFUNN(signum,1)
LISPFUNN(sin,1)
LISPFUNN(cos,1)
LISPFUNN(tan,1)
LISPFUNN(cis,1)
LISPFUNN(asin,1)
LISPFUNN(acos,1)
LISPFUN(atan,1,1,norest,nokey,0,NIL)
LISPFUNN(sinh,1)
LISPFUNN(cosh,1)
LISPFUNN(tanh,1)
LISPFUNN(asinh,1)
LISPFUNN(acosh,1)
LISPFUNN(atanh,1)
LISPFUN(float,1,1,norest,nokey,0,NIL)
LISPFUNN(rational,1)
LISPFUNN(rationalize,1)
LISPFUNN(numerator,1)
LISPFUNN(denominator,1)
LISPFUN(floor,1,1,norest,nokey,0,NIL)
LISPFUN(ceiling,1,1,norest,nokey,0,NIL)
LISPFUN(truncate,1,1,norest,nokey,0,NIL)
LISPFUN(round,1,1,norest,nokey,0,NIL)
LISPFUNN(mod,2)
LISPFUNN(rem,2)
LISPFUN(ffloor,1,1,norest,nokey,0,NIL)
LISPFUN(fceiling,1,1,norest,nokey,0,NIL)
LISPFUN(ftruncate,1,1,norest,nokey,0,NIL)
LISPFUN(fround,1,1,norest,nokey,0,NIL)
LISPFUNN(decode_float,1)
LISPFUNN(scale_float,2)
LISPFUNN(float_radix,1)
LISPFUN(float_sign,1,1,norest,nokey,0,NIL)
LISPFUN(float_digits,1,1,norest,nokey,0,NIL)
LISPFUNN(float_precision,1)
LISPFUNN(integer_decode_float,1)
LISPFUN(complex,1,1,norest,nokey,0,NIL)
LISPFUNN(realpart,1)
LISPFUNN(imagpart,1)
LISPFUN(logior,0,0,rest,nokey,0,NIL)
LISPFUN(logxor,0,0,rest,nokey,0,NIL)
LISPFUN(logand,0,0,rest,nokey,0,NIL)
LISPFUN(logeqv,0,0,rest,nokey,0,NIL)
LISPFUNN(lognand,2)
LISPFUNN(lognor,2)
LISPFUNN(logandc1,2)
LISPFUNN(logandc2,2)
LISPFUNN(logorc1,2)
LISPFUNN(logorc2,2)
LISPFUNN(boole,3)
LISPFUNN(lognot,1)
LISPFUNN(logtest,2)
LISPFUNN(logbitp,2)
LISPFUNN(ash,2)
LISPFUNN(logcount,1)
LISPFUNN(integer_length,1)
LISPFUNN(byte,2)
LISPFUNN(bytesize,1)
LISPFUNN(byteposition,1)
LISPFUNN(ldb,2)
LISPFUNN(ldb_test,2)
LISPFUNN(mask_field,2)
LISPFUNN(dpb,3)
LISPFUNN(deposit_field,3)
LISPFUN(random,1,1,norest,nokey,0,NIL)
LISPFUN(make_random_state,0,1,norest,nokey,0,NIL)
LISPFUNN(fakultaet,1)
LISPFUNN(exquo,2)
LISPFUNN(long_float_digits,0)
LISPFUNN(set_long_float_digits,1)
LISPFUNN(log2,1)
LISPFUNN(log10,1)
# ---------- REXX ----------
#ifdef REXX
LISPFUN(rexx_put,1,0,norest,key,5,
        (kw(result),kw(string),kw(token),kw(host),kw(io)) )
LISPFUNN(rexx_wait_input,0)
LISPFUNN(rexx_get,0)
LISPFUNN(rexx_reply,3)
#endif
# ---------- FOREIGN ----------
#ifdef DYNAMIC_FFI
LISPFUNN(validp,1)
LISPFUNN(sizeof,1)
LISPFUNN(bitsizeof,1)
LISPFUNN(lookup_foreign_variable,2)
LISPFUNN(foreign_value,1)
LISPFUNN(set_foreign_value,2)
LISPFUNN(foreign_type,1)
LISPFUNN(foreign_size,1)
LISPFUN(element,1,0,rest,nokey,0,NIL)
LISPFUNN(deref,1)
LISPFUNN(slot,2)
LISPFUNN(cast,2)
LISPFUNN(offset,3)
LISPFUNN(lookup_foreign_function,2)
LISPFUN(foreign_call_out,1,0,rest,nokey,0,NIL)
#ifdef AMIGAOS
LISPFUN(foreign_library,1,1,norest,nokey,0,NIL)
LISPFUNN(foreign_library_variable,4)
LISPFUNN(foreign_library_function,4)
#endif
#endif
#ifdef HAVE_AFFI
LISPFUN(affi_libcall,2,0,rest,nokey,0,NIL)
LISPFUN(mem_read,2,1,norest,nokey,0,NIL)
LISPFUN(mem_write,3,1,norest,nokey,0,NIL)
LISPFUN(mem_write_vector,2,1,norest,nokey,0,NIL)
LISPFUN(affi_nonzerop,1,0,norest,nokey,0,NIL)
#endif

