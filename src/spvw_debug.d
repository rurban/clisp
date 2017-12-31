/* Debugging utilities. */

/* DEBUG_SPVW_ASSERT(expression) is an assertion used to debug SPVW. */
#ifdef DEBUG_SPVW
  /* print the failed assertion before abort() as is and with expanded macros */
  #define DEBUG_SPVW_ASSERT(expression)  if (!(expression)) {fprintf(stderr,"\n[%s:%d] assertion failure:\n" #expression "\n" STRING(expression) "\n",__FILE__,__LINE__); abort();} else (void)0/*;*/
#else
  #define DEBUG_SPVW_ASSERT(expression)  (void)0/*;*/
#endif

/* Output a memory range in hexadecimal notation */
local const char hex_table[] = "0123456789ABCDEF";
local void mem_hex_out (const void* buf, uintL count) {
  if (count > 0) {
    DYNAMIC_ARRAY(cbuf,char,3*count+1);
    const uintB* ptr1 = (const uintB*) buf;
    char* ptr2 = &cbuf[0];
    dotimespL(count,count, {
      *ptr2++ = ' ';
      *ptr2++ = hex_table[floor(*ptr1,16)]; *ptr2++ = hex_table[*ptr1 % 16];
      ptr1++;
    });
    *ptr2 = '\0';
    fprint(stdout,cbuf);
    FREE_DYNAMIC_ARRAY(cbuf);
  }
}

/* Output a lisp object in lisp notation to standard output.
 can trigger GC */
modexp maygc object object_out (object obj) {
  pushSTACK(obj);
  pushSTACK(var_stream(S(terminal_io),strmflags_wr_ch_B)); /* *TERMINAL-IO* */
  prin1(&STACK_0,STACK_1);      /* output the object */
  terpri(&STACK_0);             /* output a newline */
  skipSTACK(1);
  return popSTACK();            /* return the same object */
}

#ifdef ENABLE_UNICODE
/* see string_to_asciz() */
local void string_out_ (FILE* out, object str, object encoding) {
  var uintL len;
  var uintL offset;
  var object string = unpack_string_ro(str,&len,&offset);
  var const chart* srcptr;
  unpack_sstring_alloca(string,len,offset, srcptr=);
  var uintL bytelen = cslen(encoding,srcptr,len);
  var DYNAMIC_ARRAY(buffer,uintB,bytelen+1);
  cstombs(encoding,srcptr,len,buffer,bytelen);
  buffer[bytelen] = 0;
  fprint(out,(const char*)buffer);
  FREE_DYNAMIC_ARRAY(buffer);
}
#define string_out(o,s) string_out_(o,s,O(terminal_encoding))
#else /* no ENABLE_UNICODE */
local void string_out (FILE* out, object str) {
  var uintL len;
  var uintL offset;
  var object string = unpack_string_ro(str,&len,&offset);
  var const chart* srcptr;
  unpack_sstring_alloca(string,len,offset, srcptr=);
  var DYNAMIC_ARRAY(buffer,uintB,len+1);
  var uintB* destptr = buffer;
  while (len--) *destptr++ = as_cint(*srcptr++);
  *destptr++ = '\0'; /* append NUL byte */
  fprint(out,(const char*)buffer);
  FREE_DYNAMIC_ARRAY(buffer);
}
#endif

/* the recursive helper for nobject_out() which does all the work
 must be enclosed in begin_system_call()/end_system_call() */
local void nobject_out1 (FILE* out, object obj, int level) {
  if (level) --level;
  else { fprint(out,"<...>"); return; }
 #define XOUT(x) nobject_out1(out,x,level)
  if (stringp(obj)) {
    fprint(out,"\"");
    string_out(out,obj);
    fprint(out,"\"");
  } else if (charp(obj)) {
    object name = char_name(char_code(obj));
    fprintf(out,"[%c]",(int)as_cint(char_code(obj)));
    if (!nullp(name)) {
      fprint(out,"=#\\");
      string_out(out,name);
    }
  } else if (symbolp(obj)) {
    object symbol = symbol_without_flags(obj);
    object pack = Symbol_package(symbol);
    if (nullp(pack)) fprint(out,"#:"); /* uninterned symbol */
    else if (eq(pack,O(keyword_package))) fprint(out,":");
    else {
      string_out(out,ThePackage(pack)->pack_shortest_name);
      fprint(out,"::");
    }
    string_out(out,Symbol_name(symbol));
  } else if (simple_vector_p(obj)) {
    uintL len = vector_length(obj);
    uintL elt_index = 0;
    fprint(out,"#(");
    while (elt_index < len) {
      if (elt_index) fprint(out," ");
      XOUT(TheSvector(obj)->data[elt_index++]);
    }
    fprint(out,")");
  } else if (consp(obj)) {
    fprint(out,"(");
    while (1) {
      XOUT(Car(obj));
      obj = Cdr(obj);
      if (atomp(obj)) break;
      fprint(out," ");
    }
    if (!nullp(obj)) {
      fprint(out," . ");
      XOUT(obj);
    }
    fprint(out,")");
  } else if (arrayp(obj)) {
    fprintf(out,"#<array %lu",(unsigned long)Array_type(obj));
    if (mdarrayp(obj))
      fprintf(out," rank=%lu",(unsigned long)Iarray_rank(obj));
    else
      fprintf(out," len=%lu",(unsigned long)vector_length(obj));
    fprintf(out," 0x%lx>",as_oint(obj));
  } else if (functionp(obj)) {
    fprint(out,"#<");
    if (subrp(obj)) {
      string_out(out, (((as_oint(subr_tab_ptr_as_object(&subr_tab)) <=
                         as_oint(obj))
                        && (as_oint(obj) <
                            as_oint(subr_tab_ptr_as_object(&subr_tab+1))))
                       ? O(printstring_subr) : O(printstring_addon_subr)));
      obj = TheSubr(obj)->name;
    } else if (cclosurep(obj)) {
      if (Closure_instancep(obj))
        fprint(out,"FUNCALLABLE-INSTANCE");
      else
        string_out(out, O(printstring_compiled_closure));
      obj = Closure_name(obj);
    }
    #ifdef DYNAMIC_FFI
    else if (ffunctionp(obj)) {
      if (!fp_validp(TheFpointer(TheFaddress(TheFfunction(obj)->ff_address)->fa_base)))
        string_out(out,O(printstring_invalid));
      string_out(out,O(printstring_ffunction));
      obj = TheFfunction(obj)->ff_name;
    }
    #endif
    else { /* interpreted closure */
      string_out(out,O(printstring_closure));
      obj = TheIclosure(obj)->clos_name;
    }
    fprint(out," ");
    XOUT(obj);
    fprint(out,">");
  } else if (fsubrp(obj)) {
    fprint(out,"#<");
    string_out(out,O(printstring_fsubr));
    fprint(out," ");
    XOUT(TheFsubr(obj)->name);
    fprint(out,">");
  } else if (pathnamep(obj)) {
    fprint(out,"#("); XOUT(S(pathname));
   #define SLOT(s) fprint(out," "); XOUT(S(K##s)); fprint(out," "); \
     XOUT(ThePathname(obj)->pathname_##s)
   #if HAS_HOST
    SLOT(host);
   #endif
   #if HAS_DEVICE
    SLOT(device);
   #endif
    SLOT(directory); SLOT(name); SLOT(type); SLOT(version);
   #undef SLOT
    fprint(out,")");
  } else if (logpathnamep(obj)) {
    fprint(out,"#("); XOUT(S(logical_pathname));
   #define SLOT(s) fprint(out," "); XOUT(S(K##s)); fprint(out," "); \
     XOUT(TheLogpathname(obj)->pathname_##s)
    SLOT(host); SLOT(directory); SLOT(name); SLOT(type); SLOT(version);
   #undef SLOT
    fprint(out,")");
  } else if (hash_table_p(obj)) {
    fprint(out,"#("); XOUT(S(hash_table));
    fprintf(out," size=%lu maxcount=%lu mincount=%lu\n",
            (unsigned long)TheHashtable(obj)->ht_size,
            (unsigned long)posfixnum_to_V(TheHashtable(obj)->ht_maxcount),
            (unsigned long)posfixnum_to_V(TheHashtable(obj)->ht_mincount));
    fprint(out,"  test=");
    if (ht_test_code_user_p(ht_test_code(record_flags(TheHashtable(obj))))) {
      XOUT(TheHashtable(obj)->ht_test); fprint(out,"/");
      XOUT(TheHashtable(obj)->ht_hash);
    } else {
      switch (ht_test_code(record_flags(TheHashtable(obj))) & (bit(1)|bit(0))) {
        case 0: { XOUT(S(eq)); break; }
        case 1: { XOUT(S(eql)); break; }
        case 2: { XOUT(S(equal)); break; }
        case 3: { XOUT(S(equalp)); break; }
        default: abort();
      }
    }
    fprint(out,"\n  KV="); XOUT(TheHashtable(obj)->ht_kvtable);
    fprint(out,")");
  } else if (packagep(obj)) {
    fprint(out,"#<");
    string_out(out,O(printstring_package));
    fprint(out," ");
    string_out(out,ThePackage(obj)->pack_name);
    fprint(out,">");
  } else if (weakpointerp(obj)) {
    fprint(out,"#<");
    string_out(out,O(printstring_weakpointer));
    fprint(out," ");
    XOUT(TheWeakpointer(obj)->wp_value);
    fprint(out," ");
    if (weakpointerp(TheWeakpointer(obj)->wp_cdr)) fprint(out,"#<next wp>");
    else XOUT(TheWeakpointer(obj)->wp_cdr);
    fprintf(out," 0x%lx>",as_oint(obj));
  } else if (fpointerp(obj)) {
    fprint(out,"#<");
    if (!fp_validp(TheFpointer(obj))) string_out(out,O(printstring_invalid));
    string_out(out,O(printstring_fpointer));
    fprintf(out," 0x%lx>",(uintP)TheFpointer(obj)->fp_pointer);
  } else if (structurep(obj)) {
    uintL ii;
    fprint(out,"#<structure");
    for(ii=0; ii<Structure_length(obj); ii++) {
      fprint(out," ");
      XOUT(TheStructure(obj)->recdata[ii]);
    }
    fprintf(out," 0x%lx>",as_oint(obj));
  } else if (instancep(obj)) {
    fprint(out,"#<instance ");
    XOUT(TheInstance(obj)->inst_class_version);
    fprintf(out," 0x%lx>",as_oint(obj));
  } else if (fixnump(obj)) fprintf(out,"%ld",fixnum_to_V(obj));
  else if (eq(obj,unbound))   string_out(out,O(printstring_unbound));
  else if (eq(obj,nullobj))   fprint(out,"#<NULLOBJ>");
  else if (eq(obj,disabled))  string_out(out,O(printstring_disabled_pointer));
  else if (eq(obj,specdecl))  string_out(out,O(printstring_special_reference));
  else if (eq(obj,impdependent)) string_out(out,O(printstring_implementation_dependent));
  else if (eq(obj,eof_value)) string_out(out,O(printstring_eof));
  else if (eq(obj,dot_value)) string_out(out,O(printstring_dot));
#if defined(DYNAMIC_FFI)
  else if (faddressp(obj)) {
    fprint(out,"#<");
    if (!fp_validp(TheFpointer(TheFaddress(obj)->fa_base)))
      string_out(out,O(printstring_invalid));
    string_out(out,O(printstring_faddress)); fprint(out," ");
    XOUT(TheFaddress(obj)->fa_base);
    fprintf(out," + 0x%lx>",TheFaddress(obj)->fa_offset);
  }
#endif
  else if (framepointerp(obj)) {
    fprint(out,"#<frame ");
    switch (framecode(obj)) {
      case DYNBIND_frame_info: fprint(out,"DYNBIND"); break;
      case ENV1V_frame_info: fprint(out,"ENV1V"); break;
      case ENV1F_frame_info: fprint(out,"ENV1F"); break;
      case ENV1B_frame_info: fprint(out,"ENV1B"); break;
      case ENV1G_frame_info: fprint(out,"ENV1G"); break;
      case ENV1D_frame_info: fprint(out,"ENV1D"); break;
      case ENV2VD_frame_info: fprint(out,"ENV2VD"); break;
      case ENV5_frame_info: fprint(out,"ENV5"); break;
     #ifdef HAVE_SAVED_REGISTERS
      case CALLBACK_frame_info: fprint(out,"CALLBACK"); break;
     #endif
      case VAR_frame_info: fprint(out,"VAR"); break;
      case FUN_frame_info: fprint(out,"FUN"); break;
      case IBLOCK_frame_info: fprint(out,"IBLOCK"); break;
      case NESTED_IBLOCK_frame_info: fprint(out,"NESTED_IBLOCK"); break;
      case ITAGBODY_frame_info: fprint(out,"ITAGBODY"); break;
      case NESTED_ITAGBODY_frame_info: fprint(out,"NESTED_ITAGBODY"); break;
      case CBLOCK_CTAGBODY_frame_info: fprint(out,"CBLOCK_CTAGBODY"); break;
      case APPLY_frame_info: fprint(out,"APPLY"); break;
      case TRAPPED_APPLY_frame_info: fprint(out,"TRAPPED_APPLY"); break;
      case EVAL_frame_info: fprint(out,"EVAL"); break;
      case TRAPPED_EVAL_frame_info: fprint(out,"TRAPPED_EVAL"); break;
      case CATCH_frame_info: fprint(out,"CATCH"); break;
      case HANDLER_frame_info: fprint(out,"HANDLER"); break;
      case UNWIND_PROTECT_frame_info: fprint(out,"UNWIND_PROTECT"); break;
      case DRIVER_frame_info: fprint(out,"DRIVER"); break;
      default: fprint(out,"**UNKNOWN**");
    }
    fprintf(out," %lu>",
            (unsigned long)STACK_item_count(uTheFramepointer(obj),(gcv_object_t*)STACK_start));
  } else if (builtin_stream_p(obj)) {
    fprintf(out,"#<built-in-stream type=%d flags=%d len=%lu xlen=%lu slen=%lu",
            TheStream(obj)->strmtype,TheStream(obj)->strmflags,
            (unsigned long)Stream_length(obj),
            (unsigned long)Stream_xlength(obj),
            (unsigned long)strm_len);
    switch (TheStream(obj)->strmtype) {
      case strmtype_pphelp: fprint(out," pretty-print-help");
        fprint(out," modus="); XOUT(TheStream(obj)->strm_pphelp_modus);
        fprint(out," lpos="); XOUT(TheStream(obj)->strm_pphelp_lpos);
        fprint(out," strings="); XOUT(TheStream(obj)->strm_pphelp_strings);
        break;
      case strmtype_file: fprint(out," file");
        fprint(out," name="); XOUT(TheStream(obj)->strm_file_name);
        fprint(out," truename="); XOUT(TheStream(obj)->strm_file_truename);
        fprintf(out," channel=%d",
                TheHandle(TheStream(obj)->strm_buffered_channel));
        fprint(out," eltype="); XOUT(TheStream(obj)->strm_eltype);
        fprint(out," encoding="); XOUT(TheStream(obj)->strm_encoding);
        break;
      default: {
        int ii=0;
        for (; ii < Stream_length(obj) - strm_len; ii++) {
          fprintf(out," %d=",ii);
          XOUT(TheStream(obj)->strm_other[ii]);
        }
      }
    }
    fprintf(out," 0x%lx>",as_oint(obj));
  } else if (encodingp(obj)) {
    fprint(out,"#<encoding eol="); XOUT(TheEncoding(obj)->enc_eol);
    fprint(out," wce="); XOUT(TheEncoding(obj)->enc_towcs_error);
    fprint(out," mbe="); XOUT(TheEncoding(obj)->enc_tombs_error);
   #ifdef ENABLE_UNICODE
    fprint(out," cs="); XOUT(TheEncoding(obj)->enc_charset);
   #endif
    fprintf(out," 0x%lx>",as_oint(obj));
  }
  #ifndef TYPECODES
  else if (varobjectp(obj))
    fprintf(out,"#<varobject type=%d address=0x%lx>",
            varobject_type(TheVarobject(obj)),(uintP)ThePointer(obj));
  #endif
  else fprintf(out,"#<huh?! address=0x%lx>",(uintP)ThePointer(obj));
 #undef XOUT
}

/* non-consing, STACK non-modifying */
local int nobject_out_level = 5; /* for debugging */
global object nobject_out (FILE* out, object obj) {
  begin_system_call();
  if (out == NULL) out = stdout;
  nobject_out1(out,obj,nobject_out_level);
  fflush(out);
  end_system_call();
  return obj;
}

/* use (struct backtrace_t*) and not p_backtrace_t
   so that this is useable from the p_backtrace_t C++ definition */
local int back_trace_depth (const struct backtrace_t *bt) {
  uintL bt_index = 0;
  const struct backtrace_t *bt_fast = (bt ? bt : back_trace);
  const struct backtrace_t *bt_slow = bt_fast;
  while (bt_fast) {
    bt_fast = bt_fast->bt_next; bt_index++;
    if (bt_fast == bt_slow) return -bt_index;
    if (bt_fast) { bt_fast = bt_fast->bt_next; bt_index++; }
    if (bt_fast == bt_slow) return -bt_index;
    bt_slow = bt_slow->bt_next;
  }
  return bt_index;
}

/* print a single struct backtrace_t object
 the caller must do begin_system_call()/end_system_call() ! */
local void bt_out (FILE* out, const struct backtrace_t *bt, uintL bt_index) {
  fprintf(out,"[%lu/0x%lx]%s ",(unsigned long)bt_index,(uintP)bt,
          bt_beyond_stack_p(bt,STACK)?"<":">");
  nobject_out(out,bt->bt_function);
  if (bt->bt_num_arg >= 0)
    fprintf(out," %d args",bt->bt_num_arg);
  if (bt->bt_next)
    fprintf(out," delta: STACK=%lud; SP=%ld",
            (unsigned long)STACK_item_count(top_of_back_trace_frame(bt),
                                            top_of_back_trace_frame(bt->bt_next)),
            (((long)((char*)(bt->bt_next) - (char*)bt) ^ SPoffset) - SPoffset)
            / sizeof(SPint));
  fprint(out,"\n"); fflush(out);
}

/* print the whole backtrace stack */
local uintL back_trace_out (FILE* out, const struct backtrace_t *bt) {
  uintL bt_index = 0;
  const struct backtrace_t *bt_fast = (bt ? bt : back_trace);
  const struct backtrace_t *bt_slow = bt_fast;
  if (out == NULL) out = stdout;
  begin_system_call();
  while (bt_fast) {
    bt_out(out,bt_fast,bt_index++); bt_fast = bt_fast->bt_next;
    if (bt_fast == bt_slow) {
     circular:
      fprint(out,"*** error: backtrace circularity detected!\n");
      bt_index = -bt_index;
      break;
    }
    if (bt_fast) {
      bt_out(out,bt_fast,bt_index++);
      bt_fast = bt_fast->bt_next;
    }
    if (bt_fast == bt_slow) goto circular;
    bt_slow = bt_slow->bt_next;
  }
  end_system_call();
  return bt_index;
}

global void back_trace_check (const struct backtrace_t *bt,
                              const char* label, const char* file, int line) {
  if (bt && back_trace_depth(bt)<0) {
    fprintf(stderr,"\n%s:%d:%s: circularity!\n",file,line,label);
    back_trace_out(stderr,bt);
    abort();
  }
}

#if 0 /* These functions are only for debugging. */

/* note that the following will _NOT_ work if CLISP uses O(dynamic_string)
 for DYNAMIC_STRING() because the length of the "dynamic string" will be
 that of its latest allocation, not value of the second argument!!! */
local object find_pack (char* pack_s) {
  if (pack_s) {
    var uintL pack_s_len = asciz_length(pack_s);
    DYNAMIC_STRING(pack,pack_s_len);
    var chart* ptr = TheSnstring(pack)->data;
    while (pack_s_len--) *ptr++ = as_chart(*pack_s++);
    value1 = find_package(pack);
    FREE_DYNAMIC_STRING(pack);
    return value1;
  } else return O(default_package); /* CL */
}

local object find_sym (char* name_s, char* pack_s) {
  var object pack = find_pack(pack_s);
  if (nullp(pack)) return NIL;
  var uintL name_s_len = asciz_length(name_s);
  DYNAMIC_STRING(name,name_s_len);
  { var chart* ptr = TheSnstring(name)->data;
    while (name_s_len--) *ptr++ = as_chart(*name_s++);
  }
  pushSTACK(name); pushSTACK(pack); funcall(L(find_symbol),2);
  FREE_DYNAMIC_STRING(name);
  return value1;
}

#endif

#if defined(DEBUG_SPVW)
unsigned int get_constsym_count (void);
unsigned int get_constsym_count (void) { return symbol_count; }
object get_constsym (unsigned int);
object get_constsym (unsigned int pos) {
  if (pos < symbol_count) return symbol_tab_ptr_as_object(((symbol_*)((char*)&symbol_tab+varobjects_misaligned))+pos);
  else return Fixnum_0;
}
#define FUN(from,to,name) local to CONCAT(name,_) (from x) { return name(x); }
FUN(chart,cint,as_cint)
FUN(cint,chart,as_chart)
FUN(object,chart,char_code)
FUN(chart,object,code_char)
FUN(object,cint,char_int)
FUN(cint,object,int_char)
FUN(object,oint,as_oint)
FUN(oint,object,as_object)
FUN(object,sintB,Record_type)
FUN(object,uintB,Record_flags)
FUN(object,uintL,Record_length)
FUN(object,sintB,Array_type)
FUN(object,uintL,Srecord_length)
FUN(object,uintL,Xrecord_length)
FUN(object,uintL,Xrecord_xlength)
FUN(object,Cons,TheCons)
FUN(object,Record,TheRecord)
FUN(object,Srecord,TheSrecord)
FUN(object,Xrecord,TheXrecord)
FUN(object,void*,TheMachine)
FUN(object,Stream,TheStream)
FUN(object,object,Car)
FUN(object,object,Cdr)
FUN(object,Symbol,TheSymbol)
FUN(object,Hashtable,TheHashtable)
FUN(object,Package,ThePackage)
FUN(object,Pathname,ThePathname)
FUN(object,Dfloat,TheDfloat)
FUN(object,Lfloat,TheLfloat)
FUN(object,Cclosure,TheCclosure)
FUN(object,int,Cclosure_length)
FUN(object,Codevec,TheCodevec)
local void venv_out (FILE *out, object venv) { /* cf eval.d:symbol_env_search */
  begin_system_call();
  if (out == NULL) out = stdout;
 next_env:
  nobject_out(out,venv); fprint(out,"\n");
  if (framepointerp(venv)) {
     var gcv_object_t* FRAME = TheFramepointer(venv);
     var uintL count = as_oint(FRAME_(frame_count)); /* number of bindings */
     fprintf(out,"* count=%lu\n",(unsigned long)count);
     if (count > 0) {
      var gcv_object_t* bindingsptr = &FRAME_(frame_bindings); /* 1st binding */
      do {
       #ifdef NO_symbolflags
        nobject_out(out,*(bindingsptr STACKop 1));
       #else
        nobject_out(out,*(bindingsptr STACKop 0));
       #endif
        fprint(out,"\n");
        bindingsptr skipSTACKop varframe_binding_size; /* no: next binding */
      } while (--count);
    }
    venv = FRAME_(frame_next_env);
    goto next_env;
  }
  fflush(out);
  end_system_call();
}
#undef FUN
#endif
