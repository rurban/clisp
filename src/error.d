/*
 * Error-Handling for CLISP
 * Bruno Haible 1990-2002
 * Marcus Daniels 8.4.1994
 * Sam Steingold 1998-2004
 * German comments translated into English: Stefan Kain 2002-09-11
 */

#include "lispbibl.c"

/* SYS::*RECURSIVE-ERROR-COUNT* =
   depth of recursion of the output of error messages */

local void cancel_interrupts (void) {
 #ifdef PENDING_INTERRUPTS
  interrupt_pending = false; /* Ctrl-C pending time is soon completed */
   #ifndef WIN32_NATIVE
    begin_system_call();
     #ifdef HAVE_UALARM
      ualarm(0,0); /* abort SIGALRM timer */
     #else
      alarm(0); /* abort SIGALRM timer */
     #endif
    end_system_call();
   #endif
 #endif
}

/* UP: Starts the output of an error message.
 begin_error()
 < STACK_0: Stream (in general *ERROR-OUTPUT*)
 < STACK_1: value of *error-handler*
 < STACK_2: list of arguments for *error-handler*
 < STACK_3: type of condition (in general SIMPLE-ERROR) or NIL
 decreases STACK by 7 */
local void begin_error (void)
{
  end_system_call(); /* there is no system call running anymore */
  cancel_interrupts();
 #if defined(HAVE_SIGNALS) && defined(SIGPIPE)
  writing_to_subprocess = false;
 #endif
  /* increase error-count, if >3 abort output: */
  dynamic_bind(S(recursive_error_count),
               fixnum_inc(Symbol_value(S(recursive_error_count)),1));
  if (!posfixnump(Symbol_value(S(recursive_error_count)))) /* should be a fixnum >=0 */
    Symbol_value(S(recursive_error_count)) = Fixnum_0; /* otherwise emergency correction */
  if (posfixnum_to_L(Symbol_value(S(recursive_error_count))) > 3) {
    /* multiple nested error message. */
    Symbol_value(S(recursive_error_count)) = Fixnum_0; /* delete error count */
    /* bind *PRINT-PRETTY* to NIL (in order to save memory): */
    dynamic_bind(S(print_pretty),NIL);
    fehler(serious_condition,
           /* Note: All translations of this error message should be in
              pure ASCII, to avoid endless recursion if *terminal-encoding*
              supports only ASCII characters. */
           GETTEXT("Unprintable error message"));
  }
  var object error_handler = Symbol_value(S(error_handler)); /* *ERROR-HANDLER* */
  if (!nullp(error_handler)) { /* *ERROR-HANDER* /= NIL */
    pushSTACK(NIL); pushSTACK(NIL); pushSTACK(error_handler);
    pushSTACK(make_string_output_stream()); /* String-Output-Stream */
  } else if (nullpSv(use_clcs)) { /* SYS::*USE-CLCS* */
    /* *ERROR-HANDER* = NIL, SYS::*USE-CLCS* = NIL */
    pushSTACK(NIL); pushSTACK(NIL); pushSTACK(NIL);
    pushSTACK(var_stream(S(error_output),strmflags_wr_ch_B)); /* Stream *ERROR-OUTPUT* */
    terpri(&STACK_0); /* new line */
    write_sstring(&STACK_0,O(error_string1)); /* print "*** - " */
  } else { /* *ERROR-HANDER* = NIL, SYS::*USE-CLCS* /= NIL */
    pushSTACK(S(simple_error)); pushSTACK(NIL); pushSTACK(unbound);
    pushSTACK(make_string_output_stream()); /* String-Output-Stream */
  }
}

/* UP: output an error-object. */
local void write_errorobject (object obj) {
  if (nullp(STACK_1)) {
    dynamic_bind(S(prin_stream),unbound); /* bind SYS::*PRIN-STREAM* to #<UNBOUND> */
    dynamic_bind(S(print_escape),T); /* bind *PRINT-ESCAPE* to T */
    dynamic_bind(S(print_readably),NIL); /* bind *PRINT-READABLY* to NIL */
    prin1(&STACK_(0+3+3+3),obj); /* output directly */
    dynamic_unbind(S(print_readably));
    dynamic_unbind(S(print_escape));
    dynamic_unbind(S(prin_stream));
  } else {
    /* push obj onto the argument list: */
    pushSTACK(obj);
    obj = allocate_cons();
    Car(obj) = popSTACK();
    Cdr(obj) = STACK_2; STACK_2 = obj;
    /* and write "~S" into the format string: */
    write_ascii_char(&STACK_0,'~'); write_ascii_char(&STACK_0,'S');
  }
}

/* UP: outputs an error-character. */
local void write_errorchar (object obj) {
  if (nullp(STACK_1)) {
    write_char(&STACK_0,obj); /* write directly */
  } else { /* push obj on the argument list: */
    pushSTACK(obj);
    obj = allocate_cons();
    Car(obj) = popSTACK();
    Cdr(obj) = STACK_2; STACK_2 = obj;
    /* and write "~A" into the format string: */
    write_ascii_char(&STACK_0,'~'); write_ascii_char(&STACK_0,'A');
  }
}

/* UP: Outputs a piece of an error string without modifications.
 write_errorasciz_substring(start,end);
 > start, end: delimit an unmovable string in UTF-8 encoding */
local void write_errorasciz_substring (const uintB* start, const uintB* end)
{
 #ifdef UNICODE
  var object encoding = O(internal_encoding);
  var const uintB* bptr = start;
  var const uintB* bendptr = end;
  var uintL clen = Encoding_mblen(encoding)(encoding,bptr,bendptr);
  if (clen > 0) {
    var DYNAMIC_ARRAY(charbuf,chart,clen);
    {
      var chart* cptr = &charbuf[0];
      var chart* cendptr = cptr+clen;
      Encoding_mbstowcs(encoding)(encoding,nullobj,&bptr,
                                  bendptr,&cptr,cendptr);
      ASSERT(cptr == cendptr);
    }
    {
      var const chart* cptr = &charbuf[0];
      dotimespL(clen,clen, { write_code_char(&STACK_0,*cptr); cptr++; });
    }
    FREE_DYNAMIC_ARRAY(charbuf);
  }
 #else
  var const uintB* bptr = start;
  while (bptr != end) {
    write_code_char(&STACK_0,as_chart(*bptr));
    bptr++;
  }
 #endif
}

/* UP: Outputs an errorstring unchanged.
 write_errorasciz(asciz);
 > asciz: errorstring (a non-relocatable ASCIZ-string), in UTF-8 Encoding */
local void write_errorasciz (const char* asciz) {
  write_errorasciz_substring((const uintB*)asciz,
                             (const uintB*)(asciz + asciz_length(asciz)));
}

/* UP: Outputs an errorstring. At each tilde '~' an object from the stack
 is printed, at each '$' a character from the stack is printed.
 write_errorstring(errorstring)
 > STACK_0: Stream etc.
 > errorstring: Errorstring (an non-relocatable ASCIZ-string),
                in UTF-8 Encoding
 > STACK_7, STACK_8, ...: arguments (for each '~' resp. '$' one argument),
   in reversed order as with FUNCALL !
 < result: STACK-value above the stream and the arguments */
local gcv_object_t* write_errorstring (const char* errorstring)
{
  var gcv_object_t* argptr = args_end_pointer STACKop 7; /* pointer above stream and frame */
  loop {
    var char ch = *errorstring; /* next character */
    if (ch==0) /* string finished? */
      break;
    if (ch=='~') { /* tilde? */
      /* yes -> print an object from stack: */
      write_errorobject(BEFORE(argptr));
      errorstring++;
    } else if (ch=='$') { /* '$' ? */
      /* yes -> print a character from stack: */
      write_errorchar(BEFORE(argptr));
      errorstring++;
    } else {
      /* no -> output all characters until the next special character */
      var const char* ptr = errorstring;
      loop {
        ptr++;
        ch = *ptr;
        if (ch==0 || ch=='~' || ch=='$')
          break;
      }
      write_errorasciz_substring((const uintB*)errorstring,(const uintB*)ptr);
      errorstring = ptr;
    }
  }
  return argptr;
}

/* SIGNAL the CONDITION and INVOKE the debugger */
nonreturning_function(local, signal_and_debug, (object condition)) {
  pushSTACK(condition); /* save condition */
  dynamic_bind(S(print_escape),T); /* bind *PRINT-ESCAPE* to NIL */
  dynamic_bind(S(print_readably),NIL); /* bind *PRINT-READABLY* to NIL */
  pushSTACK(condition); funcall(L(clcs_signal),1); /* (SIGNAL condition) */
  dynamic_bind(S(prin_stream),unbound); /* bind SYS::*PRIN-STREAM* to #<UNBOUND> */
  pushSTACK(STACK_(0+3+3+3)); /* condition */
  funcall(L(invoke_debugger),1); /* (INVOKE-DEBUGGER condition) */
  NOTREACHED;
}

/* finishes the output of an error message and starts a new driver,
 (when start_driver_p is true)
 may trigger GC */
local void end_error (gcv_object_t* stackptr, bool start_driver_p) {
  if (nullp(STACK_1)) {
    /* *ERROR-HANDER* = NIL, SYS::*USE-CLCS* = NIL */
    skipSTACK(4); /* error message has already been printed */
    /* unbind binding frame for sys::*recursive-error-count*,
       because no error message output is active */
    dynamic_unbind(S(recursive_error_count));
    set_args_end_pointer(stackptr);
    break_driver(false); /* call break-driver (does not return) */
  } else {
    STACK_0 = get_output_stream_string(&STACK_0);
    var object arguments = nreverse(STACK_2);
    /* stack layout: type, args, handler, errorstring. */
    if (boundp(STACK_1)) {
      /* *ERROR-HANDER* /= NIL
         stack layout: nil, args, handler, errorstring.
         execute (apply *error-handler* nil errorstring args): */
      check_SP(); check_STACK();
      {
        var object error_handler = STACK_1; STACK_1 = NIL;
        apply(error_handler,2,arguments);
        skipSTACK(2);
      }
      /* unbind binding frame for sys::*recursive-error-count*,
         because no error message output is active */
      dynamic_unbind(S(recursive_error_count));
      set_args_end_pointer(stackptr);
      if (start_driver_p)
        break_driver(false); /* call break-driver (does not return) */
    } else {
      /* *ERROR-HANDER* = NIL, SYS::*USE-CLCS* /= NIL
         stack layout: type, args, --, errorstring. */
      var object type = STACK_3;
      var object errorstring = STACK_0;
      skipSTACK(4);
      dynamic_unbind(S(recursive_error_count));
      /* execute (APPLY #'coerce-to-condition errorstring args
                        'error type keyword-arguments) */
      pushSTACK(errorstring); pushSTACK(arguments);
      pushSTACK(S(error)); pushSTACK(type);
      var uintC argcount = 4;
      /* arithmetic-error, division-by-zero, floating-point-overflow,
         floating-point-underflow
         --> complete :operation :operands ??
         cell-error, uncound-variable, undefined-function, unbound-slot
         --> complete :name */
      if (eq(type,S(simple_cell_error))
          || eq(type,S(simple_unbound_variable))
          || eq(type,S(simple_undefined_function))
          || eq(type,S(simple_unbound_slot))) {
        pushSTACK(S(Kname)); pushSTACK(BEFORE(stackptr)); /* :name ... */
        argcount += 2;
      }
      /* unbound-slot --> complete :instance */
      if (eq(type,S(simple_unbound_slot))) {
        pushSTACK(S(Kinstance)); pushSTACK(BEFORE(stackptr)); /* :instance ... */
        argcount += 2;
      }
      /* type-error, keyword-error --> complete :datum, :expected-type */
      if (eq(type,S(simple_type_error))
          || eq(type,S(simple_keyword_error))
          || eq(type,S(simple_charset_type_error))) {
        pushSTACK(S(Kexpected_type)); pushSTACK(BEFORE(stackptr)); /* :expected-type ... */
        pushSTACK(S(Kdatum)); pushSTACK(BEFORE(stackptr)); /* :datum ... */
        argcount += 4;
      }
      /* package-error --> complete :package */
      if (eq(type,S(simple_package_error))) {
        pushSTACK(S(Kpackage)); pushSTACK(BEFORE(stackptr)); /* :package ... */
        argcount += 2;
      }
      /* print-not-readable --> complete :object */
      if (eq(type,S(simple_print_not_readable))) {
        pushSTACK(S(Kobject)); pushSTACK(BEFORE(stackptr)); /* :object */
        argcount += 2;
      }
      /* stream-error, end-of-file --> complete :stream */
      if (eq(type,S(simple_stream_error))
          || eq(type,S(simple_end_of_file))) {
        pushSTACK(S(Kstream)); pushSTACK(BEFORE(stackptr)); /* :stream ... */
        argcount += 2;
      }
      /* file-error --> complete :pathname */
      if (eq(type,S(simple_file_error))) {
        pushSTACK(S(Kpathname)); pushSTACK(BEFORE(stackptr)); /* :pathname ... */
        argcount += 2;
      }
      funcall(S(coerce_to_condition),argcount); /* SYS::COERCE-TO-CONDITION */
      set_args_end_pointer(stackptr);
      if (start_driver_p)
        signal_and_debug(value1);
    }
  }
}

/* Error message with Errorstring. Does not return.
 fehler(errortype,errorstring);
 > errortype: condition type
 > errorstring: Constant ASCIZ-string, in UTF-8 Encoding.
   At each tilde a LISP-object is taken from STACK and printed instead of
   the tilde.
 > on the STACK: initialization values for the condition,
                 according to errortype */
nonreturning_function(global, fehler, (condition_t errortype,
                                       const char* errorstring)) {
  begin_error(); /* start error message */
  if (!nullp(STACK_3)) { /* *ERROR-HANDLER* = NIL, SYS::*USE-CLCS* /= NIL ? */
    /* choose error-type-symbol for errortype: */
    var object sym = S(simple_condition); /* first error-type */
    sym = objectplus(sym,
                     (soint)(sizeof(*TheSymbol(sym))
                             <<(oint_addr_shift-addr_shift))
                     * (uintL)errortype);
    STACK_3 = sym;
  }
  end_error(write_errorstring(errorstring),true); /* finish error message */
  /* there is no point in using the condition system here:
     we will get into an infinite loop reporting the error */
  fprintf(stderr,"[%s:%d] cannot handle the fatal error due to a fatal error in the fatal error handler!\n",__FILE__,__LINE__);
  abort();
  /* NOTREACHED; */
}

/* just like fehler(), but allow recovery via STORE-VALUE / USE-VALUE
 expects one more stack element before everything cosumed by fehler() -
 the place to be modified or NIL
 the returned multiple values come from CHECK-VALUE (see condition.lisp)
 may trigger GC */
global void check_value (condition_t errortype, const char* errorstring)
{
  begin_error(); /* start error message */
  if (!nullp(STACK_3)) { /* *ERROR-HANDLER* = NIL, SYS::*USE-CLCS* /= NIL ? */
    /* choose error-type-symbol for errortype: */
    var object sym = S(simple_condition); /* first error-type */
    sym = objectplus(sym,
                     (soint)(sizeof(*TheSymbol(sym))
                             <<(oint_addr_shift-addr_shift))
                     * (uintL)errortype);
    STACK_3 = sym;
  }
  /* finish the error message */
  end_error(write_errorstring(errorstring),nullpSv(use_clcs));
  /* if SYS::*USE-CLCS* /= NIL, use CHECK-VALUE */
  pushSTACK(value1); /* place is already on the stack! */
  funcall(S(check_value),2);
}

#undef OS_error
#undef OS_file_error
#undef OS_filestream_error

#ifdef UNIX
#include "errunix.c"
#endif /* UNIX */

#ifdef WIN32_NATIVE
#include "errwin32.c"
#endif

/* Just like OS_error, but takes a channel stream and signals a FILE-ERROR.
 OS_filestream_error(stream);
 > stream: a channel stream
 > end_system_call() already called */
nonreturning_function(global, OS_filestream_error, (object stream)) {
  if (TheStream(stream)->strmtype == strmtype_file
      && !nullp(TheStream(stream)->strm_file_truename)) {
    OS_file_error(TheStream(stream)->strm_file_truename);
  } else {
    OS_error();
  }
}

LISPFUN(error,seclass_default,1,0,rest,nokey,0,NIL)
/* (ERROR errorstring {expr})
 Does not return.
 (defun error (errorstring &rest args)
   (if (or *error-handler* (not *use-clcs*))
     (progn
       (if *error-handler*
         (apply *error-handler* nil errorstring args)
         (progn
           (terpri *error-output*)
           (write-string "*** - " *error-output*)
           (apply #'format *error-output* errorstring args)))
       (funcall *break-driver* nil))
     (let ((condition (coerce-to-condition errorstring args 'error
                                           'simple-error)))
       (signal condition)
       (invoke-debugger condition)))) */
{
  if (!nullpSv(error_handler) || nullpSv(use_clcs)) {
    begin_error(); /* start error message */
    rest_args_pointer skipSTACKop 1; /* pointer to the arguments */
    {
      var object fun;
      var object arg1;
      if (nullp(STACK_1)) {
        fun = S(format); arg1 = STACK_0; /* (FORMAT *error-output* ...) */
      } else {
        fun = STACK_1; arg1 = NIL; /* (FUNCALL *error-handler* NIL ...) */
      }
      skipSTACK(4);
      /* write error message:
         (FORMAT *ERROR-OUTPUT* errorstring {expr})
         resp. ({handler} nil errorstring {expr}) */
      pushSTACK(arg1);
      {
        var gcv_object_t* ptr = rest_args_pointer;
        var uintC count;
        dotimespC(count,1+argcount, { pushSTACK(NEXT(ptr)); } );
      }
      funcall(fun,2+argcount); /* call fun (= FORMAT resp. handler) */
    }
    /* finish error message, cf. end_error(): */
    dynamic_unbind(S(recursive_error_count)); /* no error message output is active */
    set_args_end_pointer(rest_args_pointer); /* clean up STACK */
    break_driver(false); /* call break-driver (does not return) */
  } else {
    {
      var object arguments = listof(argcount);
      pushSTACK(arguments);
    }
    pushSTACK(S(error));
    pushSTACK(S(simple_error));
    funcall(S(coerce_to_condition),4); /* (SYS::COERCE-TO-CONDITION ...) */
    signal_and_debug(value1);
  }
  NOTREACHED;
}

/* (SYSTEM::%DEFCLCS error-types)
   sets the data needed for ERROR-OF-TYPE. */
LISPFUNN(defclcs,1)
{
  O(error_types) = popSTACK();
  VALUES0;
}

/* Converts a condition type into the corresponding Simple-Condition. */
local object convert_simple_condition (object type) {
  /* traverse vector O(error_types) like an Alist: */
  var object v = O(error_types);
  var uintL count = Svector_length(v);
  if (count > 0) {
    var gcv_object_t* ptr = &TheSvector(v)->data[0];
    dotimespL(count,count, {
      if (eq(type,Car(*ptr)))
        return Cdr(*ptr);
      ptr++;
    });
  }
  return type; /* not found -> leave type unchanged */
}

LISPFUN(cerror_of_type,seclass_default,3,0,rest,nokey,0,NIL)
/* (SYSTEM::CERROR-OF-TYPE continue-format-string type {keyword value}*
                           error-format-string {arg}*)
 (defun cerror-of-type (continue-format-string type &rest arguments)
   (let ((keyword-arguments '()))
     (loop
       (unless (and (consp arguments) (symbolp (car arguments))) (return))
       (push (pop arguments) keyword-arguments)
       (push (pop arguments) keyword-arguments))
     (setq keyword-arguments (nreverse keyword-arguments))
     (let ((error-format-string (first arguments))
           (args (rest arguments)))
       (apply #'cerror
         continue-format-string
         (if (or *error-handler* (not *use-clcs*))
           error-format-string
           (apply #'coerce-to-condition error-format-string args
                  'cerror (convert-simple-condition type) keyword-arguments))
         args)))) */
{
  var gcv_object_t* cfstring_ = &Next(rest_args_pointer STACKop 3);
  var uintC keyword_argcount = 0;
  rest_args_pointer skipSTACKop 1; /* pointer to the arguments behind type */
  while (argcount>=2) {
    var object next_arg = Next(rest_args_pointer); /* next argument */
    if (!symbolp(next_arg)) /* keyword? */
      break;
    rest_args_pointer skipSTACKop -2; argcount -= 2; keyword_argcount += 2;
  }
  /* next argument is hopefully a string. */
  if (!nullpSv(error_handler) || nullpSv(use_clcs)) {
    /* the type and the keyword-arguments are ignored. */
    BEFORE(rest_args_pointer) = *cfstring_;
    funcall(S(cerror),argcount+2);
    skipSTACK(keyword_argcount+1);
  } else {
    var object arguments = listof(argcount);
    /* stack layout: continue-format-string, type, {keyword, value}*,
                     errorstring.
      rearrange the stack a little bit: */
    var object errorstring = STACK_0;
    pushSTACK(NIL); pushSTACK(NIL); pushSTACK(NIL);
    {
      var gcv_object_t* ptr2 = args_end_pointer;
      var gcv_object_t* ptr1 = ptr2 STACKop 4;
      var uintC count;
      dotimesC(count,keyword_argcount, { BEFORE(ptr2) = BEFORE(ptr1); } );
      BEFORE(ptr2) = convert_simple_condition(BEFORE(ptr1));
      BEFORE(ptr2) = S(cerror);
      BEFORE(ptr2) = arguments;
      BEFORE(ptr2) = errorstring;
      BEFORE(ptr2) = arguments;
    }
    /* stack layout: continue-format-string, arguments, errorstring, args,
                     CERROR, type, {keyword, value}*. */
    funcall(S(coerce_to_condition),4+keyword_argcount); /* (SYS::COERCE-TO-CONDITION ...) */
    /* stack layout: continue-format-string, arguments. */
    arguments = STACK_0;
    STACK_0 = value1;
    apply(S(cerror),2,arguments); /* (CERROR continue-format-string condition ...) */
  }
}

LISPFUN(error_of_type,seclass_default,2,0,rest,nokey,0,NIL)
/* (SYSTEM::ERROR-OF-TYPE type {keyword value}* errorstring {expr}*)
 does not return.
 (defun error-of-type (type &rest arguments)
   ;; split off keyword arguments from the &rest arguments:
   (let ((keyword-arguments '()))
     (loop
       (unless (and (consp arguments) (symbolp (car arguments))) (return))
       (push (pop arguments) keyword-arguments)
       (push (pop arguments) keyword-arguments))
     (setq keyword-arguments (nreverse keyword-arguments))
     (let ((errorstring (first arguments))
           (args (rest arguments)))
       (if (or *error-handler* (not *use-clcs*))
         (progn
           (if *error-handler*
             (apply *error-handler* nil errorstring args)
             (progn
               (terpri *error-output*)
               (write-string "*** - " *error-output*)
               (apply #'format *error-output* errorstring args)))
           (funcall *break-driver* nil))
         (let ((condition
                 (apply #'coerce-to-condition errorstring args
                        'error (convert-simple-condition type)
                        keyword-arguments)))
           (signal condition)
           (invoke-debugger condition)))))) */
{
  var uintC keyword_argcount = 0;
  rest_args_pointer skipSTACKop 1; /* pointer to the arguments behind type */
  while (argcount>=2) {
    var object next_arg = Next(rest_args_pointer); /* next argument */
    if (!symbolp(next_arg)) /* keyword? */
      break;
    rest_args_pointer skipSTACKop -2; argcount -= 2; keyword_argcount += 2;
  }
  /* next argument is hopefully a string. */
  if (!nullpSv(error_handler) || nullpSv(use_clcs)) {
    /* the type and the keyword-arguments are ignored. */
    begin_error(); /* start error message */
    {
      var object fun;
      var object arg1;
      if (nullp(STACK_1)) {
        fun = S(format); arg1 = STACK_0; /* (FORMAT *error-output* ...) */
      } else {
        fun = STACK_1; arg1 = NIL; /* (FUNCALL *error-handler* NIL ...) */
      }
      skipSTACK(4);
      /* write error message:
         (FORMAT *ERROR-OUTPUT* errorstring {expr})
         resp. ({handler} nil errorstring {expr}) */
      pushSTACK(arg1);
      {
        var gcv_object_t* ptr = rest_args_pointer;
        var uintC count;
        dotimespC(count,1+argcount, { pushSTACK(NEXT(ptr)); } );
      }
      funcall(fun,2+argcount); /* call fun (= FORMAT resp. handler) */
    }
    /* finish error message, cf. end_error(): */
    dynamic_unbind(S(recursive_error_count)); /* no error message output is active */
    set_args_end_pointer(rest_args_pointer); /* clean up STACK */
    break_driver(false); /* call break-driver (does not return) */
  } else {
    var object arguments = listof(argcount);
    /* stack layout: type, {keyword, value}*, errorstring.
       rearrange the stack a little bit: */
    var object errorstring = STACK_0;
    pushSTACK(NIL); pushSTACK(NIL);
    {
      var gcv_object_t* ptr2 = args_end_pointer;
      var gcv_object_t* ptr1 = ptr2 STACKop 3;
      var uintC count;
      dotimesC(count,keyword_argcount, { BEFORE(ptr2) = BEFORE(ptr1); } );
      BEFORE(ptr2) = convert_simple_condition(BEFORE(ptr1));
      BEFORE(ptr2) = S(error);
      BEFORE(ptr2) = arguments;
      BEFORE(ptr2) = errorstring;
    }
    /* stack layout: errorstring, args, ERROR, type, {keyword, value}*. */
    funcall(S(coerce_to_condition),4+keyword_argcount); /* (SYS::COERCE-TO-CONDITION ...) */
    signal_and_debug(value1);
  }
  NOTREACHED;
}

LISPFUNN(invoke_debugger,1)
/* (INVOKE-DEBUGGER condition), CLtL2 p. 915
 does not return.
 (defun invoke-debugger (condition)
   (when *debugger-hook*
     (let ((debugger-hook *debugger-hook*)
           (*debugger-hook* nil))
       (funcall debugger-hook condition debugger-hook)))
   (funcall *break-driver* nil condition t)) */
{
  var object hook = Symbol_value(S(debugger_hook));
  if (!nullp(hook)) {
    var object condition = STACK_0;
    dynamic_bind(S(debugger_hook),NIL); /* bind *DEBUGGER-HOOK* to NIL */
    pushSTACK(condition); pushSTACK(hook); funcall(hook,2); /* call Debugger-Hook */
    dynamic_unbind(S(debugger_hook));
  }
  /* *BREAK-DRIVER* can be assumed here as /= NIL. */
  pushSTACK(NIL); pushSTACK(STACK_(0+1)); pushSTACK(T);
  funcall(Symbol_value(S(break_driver)),3); /* call break-driver */
  reset(1); /* returns unexpectedly -> back to the next loop */
  NOTREACHED;
}

/* UP: Executes a break-loop because of keyboard interrupt.
 > STACK_0 : calling function
 changes STACK, can trigger GC */
global void tast_break (void)
{
  cancel_interrupts();
 #if defined(HAVE_SIGNALS) && defined(SIGPIPE)
  writing_to_subprocess = false;
 #endif
  if (!nullpSv(error_handler) || nullpSv(use_clcs)) {
    /* simulate begin_error(), 7 elements on the STACK: */
    pushSTACK(NIL); pushSTACK(NIL); pushSTACK(NIL);
    pushSTACK(NIL); pushSTACK(NIL); pushSTACK(NIL);
    pushSTACK(var_stream(S(debug_io),strmflags_wr_ch_B)); /* Stream *DEBUG-IO* */
    terpri(&STACK_0); /* new line */
    write_sstring(&STACK_0,O(error_string1)); /* print "*** - " */
    /* print string, consume caller names, clean up STACK: */
    set_args_end_pointer(write_errorstring(GETTEXT("~: User break")));
    break_driver(true); /* call break-driver */
  } else {
    pushSTACK(CLSTEXT("Continue execution"));
    pushSTACK(S(simple_interrupt_condition)); /* SYSTEM::[SIMPLE-]INTERRUPT-CONDITION */
    pushSTACK(CLSTEXT("~S: User break"));
    pushSTACK(STACK_(0+3)); /* caller */
    funcall(L(cerror_of_type),4); /* (SYS::CERROR-OF-TYPE "..." 'SYSTEM::[SIMPLE-]INTERRUPT-CONDITION "..." caller) */
    skipSTACK(1);
  }
}

LISPFUN(clcs_signal,seclass_default,1,0,rest,nokey,0,NIL)
/* (SIGNAL datum {arg}*), CLtL2 p. 888
 (defun signal (datum &rest arguments)
   (let ((condition ; CLtL2 p. 918 specifies this
           (coerce-to-condition datum arguments 'signal
                                'simple-condition)))
     (when (typep condition *break-on-signals*)
       ; Enter the debugger prior to signalling the condition
       (restart-case (invoke-debugger condition)
         (continue ())))
     (invoke-handlers condition)
     nil)) */
{
  {
    var object arguments = listof(argcount);
    pushSTACK(arguments);
  }
  pushSTACK(S(clcs_signal));
  pushSTACK(S(simple_condition));
  funcall(S(coerce_to_condition),4); /* (SYS::COERCE-TO-CONDITION ...) */
  pushSTACK(value1); /* save condition */
  pushSTACK(value1); pushSTACK(Symbol_value(S(break_on_signals)));
  funcall(S(safe_typep),2); /* (SYS::SAFE-TYPEP condition *BREAK-ON-SIGNALS*) */
  if (!nullp(value1)) {
    /* call break-driver: (funcall *break-driver* t condition t)
       *BREAK-DRIVER* can be assumed here as /= NIL . */
    pushSTACK(T); pushSTACK(STACK_(0+1)); pushSTACK(T);
    funcall(Symbol_value(S(break_driver)),3);
  }
  var object condition = popSTACK(); /* condition back */
  invoke_handlers(condition); /* call handler */
  VALUES1(NIL);
}

#ifdef FOREIGN
/* check that the argument is a valid foreign pointer.
 check_fpointer(obj);
 > obj: object
 > restart_p: allow entering a new value
 < a valid foreign pointer
 this is used by foreign.d and also by some modules
 that rely on fpointer but not FFI, e.g., regexp
 can trigger GC */
global object check_fpointer (object obj, bool restart_p) {
 check_fpointer_restart:
  if (!fpointerp(obj)) {
    pushSTACK(NIL);                /* no PLACE */
    pushSTACK(obj);                /* TYPE-ERROR slot DATUM */
    pushSTACK(S(foreign_pointer)); /* TYPE-ERROR slot EXPECTED-TYPE */
    pushSTACK(S(foreign_pointer)); pushSTACK(obj);
    pushSTACK(TheSubr(subr_self)->name);
    if (restart_p)
      check_value(type_error,GETTEXT("~: ~ is not a ~"));
    else fehler(type_error,GETTEXT("~: ~ is not a ~"));
    obj = value1;
    goto check_fpointer_restart;
  }
  if (!fp_validp(TheFpointer(obj))) {
    pushSTACK(NIL);                /* no PLACE */
    pushSTACK(obj); pushSTACK(TheSubr(subr_self)->name);
    if (restart_p)
      check_value(error,GETTEXT("~: ~ comes from a previous Lisp session and is invalid"));
    else fehler(error,GETTEXT("~: ~ comes from a previous Lisp session and is invalid"));
    obj = value1;
    goto check_fpointer_restart;
  }
  return obj;
}
#endif

/* error-message, if an object is not a list.
 fehler_list(obj);
 > obj: non-list */
nonreturning_function(global, fehler_list, (object obj)) {
  pushSTACK(obj);     /* TYPE-ERROR slot DATUM */
  pushSTACK(S(list)); /* TYPE-ERROR slot EXPECTED-TYPE */
  pushSTACK(S(list)); pushSTACK(obj); pushSTACK(TheSubr(subr_self)->name);
  fehler(type_error,GETTEXT("~: ~ is not a ~"));
}
/* ditto - recoverable
 can trigger GC */
global object check_list (object obj) {
  while (!listp(obj)) {
    pushSTACK(NIL); /* no PLACE */
    pushSTACK(obj);     /* TYPE-ERROR slot DATUM */
    pushSTACK(S(list)); /* TYPE-ERROR slot EXPECTED-TYPE */
    pushSTACK(S(list)); pushSTACK(obj); pushSTACK(TheSubr(subr_self)->name);
    check_value(type_error,GETTEXT("~: ~ is not a ~"));
    obj = value1;
  }
  return obj;
}

/* error-message, if an object is not a true list.
 fehler_proper_list(caller,obj);
 > caller: the caller (a symbol)
 > obj: end of the list, non-list */
nonreturning_function(global, fehler_proper_list, (object caller, object obj))
{
  pushSTACK(obj);     /* TYPE-ERROR slot DATUM */
  pushSTACK(S(list)); /* TYPE-ERROR slot EXPECTED-TYPE */
  pushSTACK(obj); pushSTACK(caller);
  fehler(type_error,GETTEXT("~: A true list must not end with ~"));
}

/* UP: error, if an object is not a symbol.
 check_symbol(obj);
 > obj: maybe non-symbol
 < symbol
 can trigger GC */
global object check_symbol (object sy) {
  while (!symbolp(sy)) {
    var object caller = subr_self;
    caller = (subrp(caller) ? TheSubr(caller)->name : TheFsubr(caller)->name);
    pushSTACK(NIL); /* no PLACE */
    pushSTACK(sy);        /* TYPE-ERROR slot DATUM */
    pushSTACK(S(symbol)); /* TYPE-ERROR slot EXPECTED-TYPE */
    pushSTACK(S(symbol)); pushSTACK(sy); pushSTACK(caller);
    check_value(type_error,GETTEXT("~: ~ is not a ~"));
    sy = value1;
  }
  return sy;
}

/* UP: signal an error if OBJ is not a non-constant symbol and
 return OBJ otherwise
 > obj: a potential symbol
 > caller: a symbol
 < obj: a non-constant symbol
 can trigger GC */
global object check_symbol_non_constant (object obj, object caller)
{
  while (1) {
    obj = check_symbol(obj);
    if (!constantp(TheSymbol(obj))) break;
    pushSTACK(NIL); /* no PLACE */
    pushSTACK(obj);
    pushSTACK(caller);
    check_value(source_program_error,
                GETTEXT("~: ~ is a constant, may not be used as a variable"));
    obj = value1;
  }
  return obj;
}

/* YP: signal an error if a non-symbol was declared special
 returns the symbol
 can trigger GC */
global object check_symbol_special (object obj, object caller)
{
  while (!symbolp(obj)) {
    pushSTACK(caller);
    pushSTACK(NIL); /* no PLACE */
    pushSTACK(S(special)); pushSTACK(obj); pushSTACK(caller);
    check_value(source_program_error,
                GETTEXT("~: ~ is not a symbol, cannot be declared ~"));
    caller = popSTACK();
    obj = value1;
  }
  return obj;
}

/* error-message, if an object is not a simple-vector.
 fehler_kein_svector(caller,obj);
 > caller: caller (a symbol)
 > obj: non-Svector */
nonreturning_function(global, fehler_kein_svector, (object caller, object obj))
{
  pushSTACK(obj);              /* TYPE-ERROR slot DATUM */
  pushSTACK(S(simple_vector)); /* TYPE-ERROR slot EXPECTED-TYPE */
  pushSTACK(S(simple_vector)); pushSTACK(obj); pushSTACK(caller);
  fehler(type_error,GETTEXT("~: ~ is not a ~"));
}

/* error-message, if an object is not a vector.
 fehler_vector(obj);
 > obj: non-vector */
nonreturning_function(global, fehler_vector, (object obj)) {
  pushSTACK(obj);       /* TYPE-ERROR slot DATUM */
  pushSTACK(S(vector)); /* TYPE-ERROR slot EXPECTED-TYPE */
  pushSTACK(S(vector)); pushSTACK(obj); pushSTACK(TheSubr(subr_self)->name);
  fehler(type_error,GETTEXT("~: ~ is not a ~"));
}

/* error-message, if an object is not an environment.
 fehler_environment(obj);
 > obj: non-vector */
nonreturning_function(global, fehler_environment, (object obj)) {
  pushSTACK(obj);              /* TYPE-ERROR slot DATUM */
  pushSTACK(O(type_svector5)); /* TYPE-ERROR slot EXPECTED-TYPE */
  pushSTACK(obj); pushSTACK(TheSubr(subr_self)->name);
  fehler(type_error,GETTEXT("~: ~ may not be used as an environment"));
}

/* error-message, if an argument is not a Fixnum >=0 :
 fehler_posfixnum(obj);
 > obj: the erroneous argument */
nonreturning_function(global, fehler_posfixnum, (object obj)) {
  pushSTACK(obj);               /* TYPE-ERROR slot DATUM */
  pushSTACK(O(type_posfixnum)); /* TYPE-ERROR slot EXPECTED-TYPE */
  pushSTACK(obj); pushSTACK(TheSubr(subr_self)->name);
  fehler(type_error,GETTEXT("~: argument ~ should be a nonnegative fixnum"));
}
/* < posfixnum
   can trigger GC */
global object check_posfixnum (object obj) {
  while (!posfixnump(obj)) {
    pushSTACK(NIL); /* no PLACE */
    pushSTACK(obj);               /* TYPE-ERROR slot DATUM */
    pushSTACK(O(type_posfixnum)); /* TYPE-ERROR slot EXPECTED-TYPE */
    pushSTACK(obj); pushSTACK(TheSubr(subr_self)->name);
    check_value(type_error,GETTEXT("~: argument ~ should be a nonnegative fixnum"));
    obj = value1;
  }
  return obj;
}

/* check_integer(obj) checks, if obj is a integer number.
 < integer number
 can trigger GC */
global object check_integer (object obj) {
  while (!integerp(obj)) {
    pushSTACK(NIL);             /* no PLACE */
    pushSTACK(obj);             /* TYPE-ERROR slot DATUM */
    pushSTACK(S(integer));      /* TYPE-ERROR slot EXPECTED-TYPE */
    pushSTACK(S(integer)); pushSTACK(obj);
    pushSTACK(TheSubr(subr_self)->name);
    check_value(type_error,GETTEXT("~: ~ is not a ~"));
    obj = value1;
  }
  return obj;
}
global object check_pos_integer (object obj) {
  while (!integerp(obj) || R_minusp(obj)) {
    pushSTACK(NIL);                /* no PLACE */
    pushSTACK(obj);                /* TYPE-ERROR slot DATUM */
    pushSTACK(O(type_posinteger)); /* TYPE-ERROR slot EXPECTED-TYPE */
    pushSTACK(S(integer)); pushSTACK(obj);
    pushSTACK(TheSubr(subr_self)->name);
    check_value(type_error,GETTEXT("~: ~ is not a non-negatve ~"));
    obj = value1;
  }
  return obj;
}

/* error-message, if an argument is not a Character:
 fehler_char(obj);
 > obj: the erroneous argument */
nonreturning_function(global, fehler_char, (object obj)) {
  pushSTACK(obj);          /* TYPE-ERROR slot DATUM */
  pushSTACK(S(character)); /* TYPE-ERROR slot EXPECTED-TYPE */
  pushSTACK(S(character)); pushSTACK(obj);
  pushSTACK(TheSubr(subr_self)->name);
  fehler(type_error,GETTEXT("~: argument ~ is not a ~"));
}
/* can trigger GC */
global object check_char (object obj) {
  while (!charp(obj)) {
    pushSTACK(NIL); /* no PLACE */
    pushSTACK(obj);          /* TYPE-ERROR slot DATUM */
    pushSTACK(S(character)); /* TYPE-ERROR slot EXPECTED-TYPE */
    pushSTACK(S(character)); pushSTACK(obj);
    pushSTACK(TheSubr(subr_self)->name);
    check_value(type_error,GETTEXT("~: argument ~ is not a ~"));
    obj = value1;
  }
  return obj;
}

/* error-message, if an argument is not a String:
 > obj: the erroneous argument */
global object check_string (object obj) {
  while (!stringp(obj)) {
    /* (VECTOR NIL) is a string, so #A(NIL (0)) is acceptable instead of "" */
    if (nil_vector_0_p(obj))
      return O(empty_string);
    pushSTACK(NIL); /* no PLACE */
    pushSTACK(obj);       /* TYPE-ERROR slot DATUM */
    pushSTACK(S(string)); /* TYPE-ERROR slot EXPECTED-TYPE */
    pushSTACK(S(string)); pushSTACK(obj); pushSTACK(TheSubr(subr_self)->name);
    check_value(type_error,GETTEXT("~: argument ~ is not a ~"));
    obj = value1;
  }
  return obj;
}

/* error-message, if an argument is not a Simple-String:
 > obj: the erroneous argument */
nonreturning_function(global, fehler_sstring, (object obj)) {
  pushSTACK(obj);              /* TYPE-ERROR slot DATUM */
  pushSTACK(S(simple_string)); /* TYPE-ERROR slot EXPECTED-TYPE */
  pushSTACK(S(simple_string)); pushSTACK(obj);
  pushSTACK(TheSubr(subr_self)->name);
  fehler(type_error,GETTEXT("~: argument ~ is not a ~"));
}

#ifndef TYPECODES
/* error-message, if a Simple-String is immutable:
 fehler_sstring_immutable(obj);
 > obj: the String */
nonreturning_function(global, fehler_sstring_immutable, (object obj)) {
  pushSTACK(obj);
  fehler(error,GETTEXT("Attempt to modify a read-only string: ~"));
}
#endif

/* Error message, if an argument is not of type (OR STRING INTEGER).
 fehler_string_integer(obj)  */
nonreturning_function(global, fehler_string_integer, (object obj)) {
  pushSTACK(obj);                    /* TYPE-ERROR slot DATUM */
  pushSTACK(O(type_string_integer)); /* TYPE-ERROR slot EXPECTED-TYPE */
  pushSTACK(obj); pushSTACK(TheSubr(subr_self)->name);
  fehler(type_error,
         GETTEXT("~: argument ~ is neither a string nor an integer"));
}

/* error-message, if an argument is not a Stream:
 fehler_stream(obj);
 > obj: the erroneous argument */
nonreturning_function(global, fehler_stream, (object obj)) {
  pushSTACK(obj);       /* TYPE-ERROR slot DATUM */
  pushSTACK(S(stream)); /* TYPE-ERROR slot EXPECTED-TYPE */
  pushSTACK(S(stream)); pushSTACK(obj); pushSTACK(TheSubr(subr_self)->name);
  fehler(type_error,GETTEXT("~: argument ~ is not a ~"));
}

/* error-message, if an argument is not a Stream of required stream type:
 fehler_streamtype(obj,type);
 > obj: the erroneous argument
 > type: required stream-type */
nonreturning_function(global, fehler_streamtype, (object obj, object type)) {
  pushSTACK(obj);  /* TYPE-ERROR slot DATUM */
  pushSTACK(type); /* TYPE-ERROR slot EXPECTED-TYPE */
  pushSTACK(type); pushSTACK(obj); pushSTACK(TheSubr(subr_self)->name);
  fehler(type_error,GETTEXT("~: argument ~ should be a stream of type ~"));
}

/* Report an error when the argument is not an encoding:
 > obj: the (possibly) bad argument
 > default: what to return for :DEFAULT
 > keyword_p: true if the object comes from the :EXTERNAL-FORMAT argument
 < encoding
 can trigger GC */
global object check_encoding (object arg, const gcv_object_t *e_default,
                              bool keyword_p) {
 restart:
  if (!boundp(arg) || eq(arg,S(Kdefault)))
    return *e_default;
  if (encodingp(arg))
    return arg;
 #ifdef UNICODE
  if (symbolp(arg) && constantp(TheSymbol(arg))
      && encodingp(Symbol_value(arg)))
    return Symbol_value(arg);
  #ifdef HAVE_GOOD_ICONV
  if (stringp(arg)) {   /* (make-encoding :charset arg) */
    pushSTACK(arg);     /* :charset */
    pushSTACK(unbound); /* :line-terminator */
    pushSTACK(unbound); /* :input-error-action */
    pushSTACK(unbound); /* :output-error-action */
    pushSTACK(unbound); /* :if-does-not-exist */
    C_make_encoding();
    return value1;
  }
  #endif
 #else
  /* This is a hack to get away without an error. */
  if (symbolp(arg) && eq(Symbol_package(arg),O(charset_package)))
    return O(default_file_encoding);
 #endif
  if (eq(arg,S(Kunix)) || eq(arg,S(Kmac)) || eq(arg,S(Kdos))) {
    /* (make-encoding :charset default-file-encoding :line-terminator arg) */
    pushSTACK(*e_default); /* :charset */
    pushSTACK(arg);        /* :line-terminator */
    pushSTACK(unbound);    /* :input-error-action */
    pushSTACK(unbound);    /* :output-error-action */
    pushSTACK(unbound);    /* :if-does-not-exist */
    C_make_encoding();
    return value1;
  }
  pushSTACK(NIL); /* no PLACE */
  pushSTACK(arg);                     /* TYPE-ERROR slot DATUM */
  pushSTACK(O(type_external_format)); /* TYPE-ERROR slot EXPECTED-TYPE */
  pushSTACK(arg);
  if (keyword_p) pushSTACK(S(Kexternal_format));
  pushSTACK(TheSubr(subr_self)->name);
  check_value(type_error,
              keyword_p ? GETTEXT("~: illegal ~ argument ~")
              : GETTEXT("~: argument ~ is not a character set"));
  arg = value1;
  goto restart;
}

/* error-message for non-paired keyword-arguments
 fehler_key_odd(argcount,caller);
 > argcount: the number of arguments on the STACK
 > caller: function */
nonreturning_function(global, fehler_key_odd, (uintC argcount, object caller))
{
  var uintC count;
  pushSTACK(NIL); pushSTACK(NIL);
  for (count=0; count<argcount; count++) STACK_(count) = STACK_(count+2);
  STACK_(argcount) = caller;
  var object arglist = listof(argcount);
  STACK_1 = arglist;
  fehler(program_error,
         GETTEXT("~: keyword arguments in ~ should occur pairwise"));
}

/* error-message for flawed keyword
 fehler_key_notkw(kw);
 > kw: Non-Symbol
 > caller: function */
nonreturning_function(global, fehler_key_notkw, (object kw, object caller)) {
  pushSTACK(kw); /* KEYWORD-ERROR slot DATUM */
  pushSTACK(S(symbol)); /* KEYWORD-ERROR slot EXPECTED-TYPE */
  pushSTACK(S(symbol)); pushSTACK(kw); pushSTACK(S(LLkey)); pushSTACK(caller);
  fehler(keyword_error,
         GETTEXT("~: ~ marker ~ is not a ~"));
}

/* error-message for flawed keyword
 fehler_key_badkw(fun,kw,kwlist);
 > fun: function
 > key: illegal keyword
 > val: its value
 > kwlist: list of legal keywords */
nonreturning_function(global, fehler_key_badkw,
                      (object fun, object key, object val, object kwlist)) {
  pushSTACK(key); /* KEYWORD-ERROR slot DATUM */
  pushSTACK(kwlist);
  pushSTACK(kwlist);
  pushSTACK(val);
  pushSTACK(key);
  pushSTACK(fun);
  { /* `(MEMBER ,@kwlist) = KEYWORD-ERROR slot EXPECTED-TYPE */
    var object type = allocate_cons();
    Car(type) = S(member); Cdr(type) = STACK_4;
    STACK_4 = type;
  }
  fehler(keyword_error,
         GETTEXT("~: illegal keyword/value pair ~, ~ in argument list."
                 NLstring "The allowed keywords are ~"));
}

/* error-message, if an argument is not a Function:
 check_function(obj);
 > obj: the erroneous argument */
global object check_function (object obj) {
  while (!functionp(obj)) {
    pushSTACK(NIL); /* no PLACE */
    pushSTACK(obj);         /* TYPE-ERROR slot DATUM */
    pushSTACK(S(function)); /* TYPE-ERROR slot EXPECTED-TYPE */
    pushSTACK(S(function)); pushSTACK(obj);
    pushSTACK(TheSubr(subr_self)->name);
    check_value(type_error,GETTEXT("~: ~ is not a ~"));
    if (symbolp(value1))
      obj = Symbol_function(value1);
    else if (funnamep(value1)) {
      var object name = get(Car(Cdr(value1)),S(setf_function));
      if (symbolp(name)) obj = Symbol_function(name);
      else obj = value1;
    } else if (consp(value1) && eq(Car(value1),S(lambda))) {
      pushSTACK(value1); pushSTACK(S(function));
      funcall(L(coerce),2);
      obj = value1;
    } else obj = value1;
  }
  return obj;
}

/* error if funname does not have a function definition
 > funname: symbol or (setf symbol)
 > caller: symbol
 < a function object
 can trigger GC */
global object check_fdefinition (object funname, object caller)
{
  var object name = (symbolp(funname) ? funname
                     : get(Car(Cdr(funname)),S(setf_function)));
  var object def = (symbolp(name) ? (object)Symbol_function(name) : unbound);
  var bool store_p = false;
  while (!functionp(def)) {
    pushSTACK(caller); pushSTACK(funname); /* save */
    pushSTACK(S(quote)); pushSTACK(funname); def = listof(2);
    pushSTACK(S(fdefinition)); pushSTACK(def); def = listof(2);
    pushSTACK(def); /* PLACE */
    pushSTACK(STACK_1/*funname*/); /* CELL-ERROR Slot NAME */
    pushSTACK(STACK_0); /* funname */
    pushSTACK(STACK_4); /* caller */
    check_value(undefined_function,GETTEXT("~: undefined function ~"));
    store_p = eq(value2,T);
    /* this is the only place where check_value()'s second value is checked
       for something other than non-NIL */
    if (eq(value2,Fixnum_0)) { /* :CONTINUE restart */
      funname = STACK_0;
      name = (symbolp(funname) ? funname
              : get(Car(Cdr(funname)),S(setf_function)));
      value1 = (symbolp(name) ? (object)Symbol_function(name) : unbound);
    }
    funname = popSTACK(); caller = popSTACK(); /* restore */
    def = value1;
  }
  if (store_p) {
    name = (symbolp(funname) ? funname
            : get(Car(Cdr(funname)),S(setf_function)));
    if (!symbolp(name)) {
      pushSTACK(Car(Cdr(funname))); /* the symbol in (setf symbol) */
      pushSTACK(def); /* save new function */
      pushSTACK(funname); funcall(S(get_funname_symbol),1);
      pushSTACK(value1); /* save new name */
      pushSTACK(value1); pushSTACK(S(setf_function)); pushSTACK(STACK_4);
      funcall(L(put),3); /* (put symbol 'setf-function name) */
      name = popSTACK(); def = popSTACK(); /* restore */
      skipSTACK(1); /* drop symbol in (setf symbol) */
    }
    Symbol_function(name) = def;
  }
  return def;
}

/* signal a type-error or source-program-error
 when the argument is not a function name
 can trigger GC */
global object check_funname (condition_t errtype, object caller, object obj) {
  pushSTACK(caller); /* save */
  while (!funnamep(obj)) {
    caller = STACK_0;
    pushSTACK(NIL); /* no PLACE */
    switch (errtype) {
      case type_error:
        pushSTACK(obj);           /* TYPE-ERROR slot DATUM */
        pushSTACK(O(type_designator_function)); /* slot EXPECTED-TYPE */
        break;
      case source_program_error: break;
      default: NOTREACHED;
    }
    pushSTACK(obj); pushSTACK(caller);
    check_value(errtype,GETTEXT("~: ~ is not a function name"));
    obj = value1;
  }
  skipSTACK(1); /* drop caller */
  return obj;
}

/* error-message, if an argument is a lambda-expression instead of a function:
 caller: caller (a symbol)
 obj: the erroneous argument */
nonreturning_function(global, fehler_lambda_expression,
                      (object caller, object obj)) {
  pushSTACK(obj);         /* TYPE-ERROR slot DATUM */
  pushSTACK(S(function)); /* TYPE-ERROR slot EXPECTED-TYPE */
  pushSTACK(obj); pushSTACK(caller);
  fehler(type_error,
         GETTEXT("~: argument ~ is not a function." NLstring "To get a function in the current environment, write (FUNCTION ...)." NLstring "To get a function in the global environment, write (COERCE '... 'FUNCTION)."));
}

/* too many arguments in a function call
 > caller : the function that is reporting the error
 > func   : the function being incorrectly called
 > ngiven : the number of arguments given
 < nmax   : the maximum number of arguments accepted */
nonreturning_function(global, fehler_too_many_args,
                      (object caller, object func, uintL ngiven, uintL nmax)) {
  pushSTACK(func);
  pushSTACK(fixnum(nmax));
  pushSTACK(fixnum(ngiven));
  if (!boundp(caller))
    fehler(program_error,GETTEXT("EVAL/APPLY: Too many arguments (~ instead of at most ~) given to ~"));
  else {
    pushSTACK(caller);
    fehler(program_error,GETTEXT("~: Too many arguments (~ instead of at most ~) given to ~"));
  }
}

/* too few arguments in a function call
 > caller : the function that is reporting the error (unbound == EVAL/APPLY)
 > func   : the function being incorrectly called
 > ngiven : the number of arguments given
 < nmin   : the minimum number of arguments required */
nonreturning_function(global, fehler_too_few_args,
                      (object caller, object func, uintL ngiven, uintL nmin)) {
  pushSTACK(func);
  pushSTACK(fixnum(nmin));
  pushSTACK(fixnum(ngiven));
  if (!boundp(caller))
    fehler(program_error,GETTEXT("EVAL/APPLY: Too few arguments (~ instead of at least ~) given to ~"));
  else {
    pushSTACK(caller);
    fehler(program_error,GETTEXT("~: Too few arguments (~ instead of at least ~) given to ~"));
  }
}

/* error, if argument is not an integer of type `uint8' .
 check_uint8(obj);
 > obj: an object
 < obj: uint8
 can trigger GC */
global object check_uint8 (object obj) {
  while (!uint8_p(obj)) {
    pushSTACK(NIL); /* no PLACE */
    pushSTACK(obj);           /* TYPE-ERROR slot DATUM */
    pushSTACK(O(type_uint8)); /* TYPE-ERROR slot EXPECTED-TYPE */
    pushSTACK(obj); pushSTACK(TheSubr(subr_self)->name);
    check_value(type_error,GETTEXT("~: ~ is not an 8-bit number"));
    obj = value1;
  }
  return obj;
}

/* error, if argument is not an integer of type `sint8' .
 check_sint8(obj);
 > obj: an object
 < obj: sint8
 can trigger GC */
global object check_sint8 (object obj) {
  while (!sint8_p(obj)) {
    pushSTACK(NIL); /* no PLACE */
    pushSTACK(obj);           /* TYPE-ERROR slot DATUM */
    pushSTACK(O(type_sint8)); /* TYPE-ERROR slot EXPECTED-TYPE */
    pushSTACK(obj); pushSTACK(TheSubr(subr_self)->name);
    check_value(type_error,GETTEXT("~: ~ is not an 8-bit number"));
    obj = value1;
  }
  return obj;
}

/* error, if argument is not an integer of type `uint16' .
 check_uint16(obj);
 > obj: an object
 < obj: uint16
 can trigger GC */
global object check_uint16 (object obj) {
  while (!uint16_p(obj)) {
    pushSTACK(NIL); /* no PLACE */
    pushSTACK(obj);            /* TYPE-ERROR slot DATUM */
    pushSTACK(O(type_uint16)); /* TYPE-ERROR slot EXPECTED-TYPE */
    pushSTACK(obj); pushSTACK(TheSubr(subr_self)->name);
    check_value(type_error,GETTEXT("~: ~ is not a 16-bit number"));
    obj = value1;
  }
  return obj;
}

/* error, if argument is not an integer of type `sint16' .
 check_sint16(obj);
 > obj: an object
 < obj: sint16
 can trigger GC */
global object check_sint16 (object obj) {
  while (!sint16_p(obj)) {
    pushSTACK(NIL); /* no PLACE */
    pushSTACK(obj);            /* TYPE-ERROR slot DATUM */
    pushSTACK(O(type_sint16)); /* TYPE-ERROR slot EXPECTED-TYPE */
    pushSTACK(obj); pushSTACK(TheSubr(subr_self)->name);
    check_value(type_error,GETTEXT("~: ~ is not a 16-bit number"));
    obj = value1;
  }
  return obj;
}

/* error, if argument is not an integer of type `uint32' .
 check_uint32(obj);
 > obj: an object
 < obj: uint32
 can trigger GC */
global object check_uint32 (object obj) {
  while (!uint32_p(obj)) {
    pushSTACK(NIL); /* no PLACE */
    pushSTACK(obj);            /* TYPE-ERROR slot DATUM */
    pushSTACK(O(type_uint32)); /* TYPE-ERROR slot EXPECTED-TYPE */
    pushSTACK(obj); pushSTACK(TheSubr(subr_self)->name);
    check_value(type_error,GETTEXT("~: ~ is not an 32-bit number"));
    obj = value1;
  }
  return obj;
}

/* error, if argument is not an integer of type `sint32' .
 check_sint32(obj);
 > obj: an object
 < obj: sint32
 can trigger GC */
global object check_sint32 (object obj) {
  while (!sint32_p(obj)) {
    pushSTACK(NIL); /* no PLACE */
    pushSTACK(obj);            /* TYPE-ERROR slot DATUM */
    pushSTACK(O(type_sint32)); /* TYPE-ERROR slot EXPECTED-TYPE */
    pushSTACK(obj); pushSTACK(TheSubr(subr_self)->name);
    check_value(type_error,GETTEXT("~: ~ is not an 32-bit number"));
    obj = value1;
  }
  return obj;
}

/* error, if argument is not an integer of type `uint64' .
 check_uint64(obj);
 > obj: an object
 < obj: uint64
 can trigger GC */
global object check_uint64 (object obj) {
  while (!uint64_p(obj)) {
    pushSTACK(NIL); /* no PLACE */
    pushSTACK(obj);            /* TYPE-ERROR slot DATUM */
    pushSTACK(O(type_uint64)); /* TYPE-ERROR slot EXPECTED-TYPE */
    pushSTACK(obj); pushSTACK(TheSubr(subr_self)->name);
    check_value(type_error,GETTEXT("~: ~ is not an 64-bit number"));
    obj = value1;
  }
  return obj;
}

/* error, if argument is not an integer of type `sint64' .
 check_sint64(obj);
 > obj: an object
 < obj: sint64
 can trigger GC */
global object check_sint64 (object obj) {
  while (!sint64_p(obj)) {
    pushSTACK(NIL); /* no PLACE */
    pushSTACK(obj);            /* TYPE-ERROR slot DATUM */
    pushSTACK(O(type_sint64)); /* TYPE-ERROR slot EXPECTED-TYPE */
    pushSTACK(obj); pushSTACK(TheSubr(subr_self)->name);
    check_value(type_error,GETTEXT("~: ~ is not an 64-bit number"));
    obj = value1;
  }
  return obj;
}

/* error, if argument is not an integer of type `uint' .
 check_uint(obj);
 > obj: an object
 < obj: uint
 can trigger GC */
global object check_uint (object obj) {
  while (!uint_p(obj)) {
    pushSTACK(NIL); /* no PLACE */
    pushSTACK(obj);            /* TYPE-ERROR slot DATUM */
   #if (int_bitsize==16)
    pushSTACK(O(type_uint16)); /* TYPE-ERROR slot EXPECTED-TYPE */
   #else /* (int_bitsize==32) */
    pushSTACK(O(type_uint32)); /* TYPE-ERROR slot EXPECTED-TYPE */
   #endif
    pushSTACK(obj); pushSTACK(TheSubr(subr_self)->name);
    check_value(type_error,GETTEXT("~: ~ is not an `unsigned int' number"));
    obj = value1;
  }
  return obj;
}

/* error, if argument is not an integer of type `sint' .
 check_sint(obj);
 > obj: an object
 < obj: sint
 can trigger GC */
global object check_sint (object obj) {
  while (!sint_p(obj)) {
    pushSTACK(NIL); /* no PLACE */
    pushSTACK(obj);            /* TYPE-ERROR slot DATUM */
   #if (int_bitsize==16)
    pushSTACK(O(type_sint16)); /* TYPE-ERROR slot EXPECTED-TYPE */
   #else /* (int_bitsize==32) */
    pushSTACK(O(type_sint32)); /* TYPE-ERROR slot EXPECTED-TYPE */
   #endif
    pushSTACK(obj); pushSTACK(TheSubr(subr_self)->name);
    check_value(type_error,GETTEXT("~: ~ is not an `int' number"));
    obj = value1;
  }
  return obj;
}

/* error, if argument is not an integer of type `ulong' .
 check_ulong(obj);
 > obj: an object
 < obj: ulong
 can trigger GC */
global object check_ulong (object obj) {
  while (!ulong_p(obj)) {
    pushSTACK(NIL); /* no PLACE */
    pushSTACK(obj);            /* TYPE-ERROR slot DATUM */
   #if (long_bitsize==32)
    pushSTACK(O(type_uint32)); /* TYPE-ERROR slot EXPECTED-TYPE */
   #else /* (long_bitsize==64) */
    pushSTACK(O(type_uint64)); /* TYPE-ERROR slot EXPECTED-TYPE */
   #endif
    pushSTACK(obj); pushSTACK(TheSubr(subr_self)->name);
    check_value(type_error,GETTEXT("~: ~ is not a `unsigned long' number"));
    obj = value1;
  }
  return obj;
}

/* error, if argument is not an integer of type `slong' .
 check_slong(obj);
 > obj: an object
 < obj: slong
 can trigger GC */
global object check_slong (object obj) {
  while (!slong_p(obj)) {
    pushSTACK(NIL); /* no PLACE */
    pushSTACK(obj);            /* TYPE-ERROR slot DATUM */
   #if (long_bitsize==32)
    pushSTACK(O(type_sint32)); /* TYPE-ERROR slot EXPECTED-TYPE */
   #else /* (long_bitsize==64) */
    pushSTACK(O(type_sint64)); /* TYPE-ERROR slot EXPECTED-TYPE */
   #endif
    pushSTACK(obj); pushSTACK(TheSubr(subr_self)->name);
    check_value(type_error,GETTEXT("~: ~ is not a `long' number"));
    obj = value1;
  }
  return obj;
}

/* error, if argument is not a Single-Float.
 check_ffloat(obj);
 > obj: an object
 < obj: Single-Float
 can trigger GC */
global object check_ffloat (object obj) {
  while (!single_float_p(obj)) {
    pushSTACK(NIL); /* no PLACE */
    pushSTACK(obj);             /* TYPE-ERROR slot DATUM */
    pushSTACK(S(single_float)); /* TYPE-ERROR slot EXPECTED-TYPE */
    pushSTACK(obj); pushSTACK(TheSubr(subr_self)->name);
    check_value(type_error,GETTEXT("~: ~ is not a single-float"));
    obj = value1;
  }
  return obj;
}

/* error, if argument is not a Double-Float.
 check_dfloat(obj);
 > obj: an object
 < obj: Double-Float
 can trigger GC */
global object check_dfloat (object obj) {
  while (!double_float_p(obj)) {
    pushSTACK(NIL); /* no PLACE */
    pushSTACK(obj);             /* TYPE-ERROR slot DATUM */
    pushSTACK(S(double_float)); /* TYPE-ERROR slot EXPECTED-TYPE */
    pushSTACK(obj); pushSTACK(TheSubr(subr_self)->name);
    check_value(type_error,GETTEXT("~: ~ is not a double-float"));
    obj = value1;
  }
  return obj;
}
