/*
 * Error-Handling for CLISP
 * Bruno Haible 1990-2002
 * Marcus Daniels 8.4.1994
 * Sam Steingold 1998-2002
 * German comments translated into English: Stefan Kain 2002-09-11
 */

#include "lispbibl.c"
#include <stdio.h>              /* declares fprintf */

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
    dynamic_unbind();
    dynamic_unbind();
    dynamic_unbind();
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
local object* write_errorstring (const char* errorstring)
{
  var object* argptr = args_end_pointer STACKop 7; /* pointer above stream and frame */
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

/* finishes the output of an error message and starts a new driver. */
nonreturning_function(local, end_error, (object* stackptr)) {
  if (nullp(STACK_1)) {
    /* *ERROR-HANDER* = NIL, SYS::*USE-CLCS* = NIL */
    skipSTACK(4); /* error message has already been printed */
    /* unbind binding frame for sys::*recursive-error-count*,
       because no error message output is active */
    dynamic_unbind();
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
      dynamic_unbind();
      set_args_end_pointer(stackptr);
      break_driver(false); /* call break-driver (does not return) */
    } else {
      /* *ERROR-HANDER* = NIL, SYS::*USE-CLCS* /= NIL
         stack layout: type, args, --, errorstring. */
      var object type = STACK_3;
      var object errorstring = STACK_0;
      skipSTACK(4);
      dynamic_unbind(); /* unbind sys::*recursive-error-count* */
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
      funcall(S(coerce_to_condition),argcount); /* (SYS::COERCE-TO-CONDITION ...) */
      /* set_args_end_pointer(stackptr); -- what for? Only makes debugging difficult! */
      signal_and_debug(value1);
    }
  }
  /* there is no point in using the condition system here:
     we will get into an infinite loop reporting the error */
  fprintf(stderr,"[%s:%d] cannot handle the fatal error due to a fatal error in the fatal error handler!\n",__FILE__,__LINE__);
  abort();
  /* NOTREACHED; */
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
  end_error(write_errorstring(errorstring)); /* write error message, stop */
}

#undef OS_error
#undef OS_file_error
#undef OS_filestream_error

#ifdef AMIGAOS
#include "erramiga.c"
#endif

#if defined(UNIX) || defined(EMUNIX) || defined(RISCOS)
#include "errunix.c"
#endif /* UNIX || EMUNIX || RISCOS */

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

LISPFUN(error,1,0,rest,nokey,0,NIL)
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
        var object* ptr = rest_args_pointer;
        var uintC count;
        dotimespC(count,1+argcount, { pushSTACK(NEXT(ptr)); } );
      }
      funcall(fun,2+argcount); /* call fun (= FORMAT resp. handler) */
    }
    /* finish error message, cf. end_error(): */
    dynamic_unbind(); /* no error message output is active */
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
    var object* ptr = &TheSvector(v)->data[0];
    dotimespL(count,count, {
      if (eq(type,Car(*ptr)))
        return Cdr(*ptr);
      ptr++;
    });
  }
  return type; /* not found -> leave type unchanged */
}

LISPFUN(cerror_of_type,3,0,rest,nokey,0,NIL)
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
  var object* cfstring_ = &Next(rest_args_pointer STACKop 3);
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
      var object* ptr2 = args_end_pointer;
      var object* ptr1 = ptr2 STACKop 4;
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

LISPFUN(error_of_type,2,0,rest,nokey,0,NIL)
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
        var object* ptr = rest_args_pointer;
        var uintC count;
        dotimespC(count,1+argcount, { pushSTACK(NEXT(ptr)); } );
      }
      funcall(fun,2+argcount); /* call fun (= FORMAT resp. handler) */
    }
    /* finish error message, cf. end_error(): */
    dynamic_unbind(); /* no error message output is active */
    set_args_end_pointer(rest_args_pointer); /* clean up STACK */
    break_driver(false); /* call break-driver (does not return) */
  } else {
    var object arguments = listof(argcount);
    /* stack layout: type, {keyword, value}*, errorstring.
       rearrange the stack a little bit: */
    var object errorstring = STACK_0;
    pushSTACK(NIL); pushSTACK(NIL);
    {
      var object* ptr2 = args_end_pointer;
      var object* ptr1 = ptr2 STACKop 3;
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
    dynamic_unbind();
  }
  /* *BREAK-DRIVER* can be assumed here as /= NIL. */
  pushSTACK(NIL); pushSTACK(STACK_(0+1)); pushSTACK(T);
  funcall(Symbol_value(S(break_driver)),3); /* call break-driver */
  reset(); /* returns unexpectedly -> back to the next loop */
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

LISPFUN(clcs_signal,1,0,rest,nokey,0,NIL)
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

/* error-message, if an object is not a list.
 fehler_list(obj);
 > obj: non-list
 > subr_self: caller (a SUBR) */
nonreturning_function(global, fehler_list, (object obj)) {
  pushSTACK(obj);     /* TYPE-ERROR slot DATUM */
  pushSTACK(S(list)); /* TYPE-ERROR slot EXPECTED-TYPE */
  pushSTACK(obj); pushSTACK(TheSubr(subr_self)->name);
  fehler(type_error,GETTEXT("~: ~ is not a list"));
}

/* error-message, if an object is not a true list.
 fehler_proper_list(obj);
 > obj: end of the list, non-list
 > subr_self: caller (a SUBR) */
nonreturning_function(global, fehler_proper_list, (object obj)) {
  pushSTACK(obj);     /* TYPE-ERROR slot DATUM */
  pushSTACK(S(list)); /* TYPE-ERROR slot EXPECTED-TYPE */
  pushSTACK(obj); pushSTACK(TheSubr(subr_self)->name);
  fehler(type_error,GETTEXT("~: A true list must not end with ~"));
}

/* error-message, if an object is not a symbol.
 fehler_kein_symbol(caller,obj);
 > caller: caller (a Symbol)
 > obj: non-symbol */
nonreturning_function(global, fehler_kein_symbol, (object caller, object obj))
{
  pushSTACK(obj);       /* TYPE-ERROR slot DATUM */
  pushSTACK(S(symbol)); /* TYPE-ERROR slot EXPECTED-TYPE */
  pushSTACK(obj);
  pushSTACK(caller);
  fehler(type_error,GETTEXT("~: ~ is not a symbol"));
}

/* error-message, if an object is not a symbol.
 fehler_symbol(obj);
 > subr_self: caller (a SUBR or FSUBR)
 > obj: non-symbol */
nonreturning_function(global, fehler_symbol, (object obj)) {
  var object aufrufer = subr_self;
  aufrufer = (subrp(aufrufer) ? TheSubr(aufrufer)->name
              : TheFsubr(aufrufer)->name);
  fehler_kein_symbol(aufrufer,obj);
}

/* UP: signal an error if OBJ is not a non-constant symbol and
 return OBJ otherwise
 > caller: the caller (function name)
 > obj: a potential symbol
 < obj: a non-constant symbol */
global object test_symbol_non_constant (object caller, object obj)
{
  if (!symbolp(obj)) fehler_kein_symbol(caller,obj);
  if (constantp(TheSymbol(obj))) {
    pushSTACK(obj); pushSTACK(caller);
    fehler(program_error,
           GETTEXT("~: ~ is a constant, may not be used as a variable"));
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
  pushSTACK(obj);
  pushSTACK(caller);
  fehler(type_error,GETTEXT("~: ~ is not a simple-vector"));
}

/* error-message, if an object is not a vector.
 fehler_vector(obj);
 > subr_self: caller (a SUBR)
 > obj: non-vector */
nonreturning_function(global, fehler_vector, (object obj)) {
  pushSTACK(obj);       /* TYPE-ERROR slot DATUM */
  pushSTACK(S(vector)); /* TYPE-ERROR slot EXPECTED-TYPE */
  pushSTACK(obj); pushSTACK(TheSubr(subr_self)->name);
  fehler(type_error,GETTEXT("~: ~ is not a vector"));
}

/* error-message, if an object is not an environment.
 fehler_environment(obj);
 > subr_self: caller (a SUBR)
 > obj: non-vector */
nonreturning_function(global, fehler_environment, (object obj)) {
  pushSTACK(obj);              /* TYPE-ERROR slot DATUM */
  pushSTACK(O(type_svector5)); /* TYPE-ERROR slot EXPECTED-TYPE */
  pushSTACK(obj); pushSTACK(TheSubr(subr_self)->name);
  fehler(type_error,GETTEXT("~: ~ may not be used as an environment"));
}

/* error-message, if an argument is not a Fixnum >=0 :
 fehler_posfixnum(obj);
 > obj: the erroneous argument
 > subr_self: caller (a SUBR) */
nonreturning_function(global, fehler_posfixnum, (object obj)) {
  pushSTACK(obj);               /* TYPE-ERROR slot DATUM */
  pushSTACK(O(type_posfixnum)); /* TYPE-ERROR slot EXPECTED-TYPE */
  pushSTACK(obj); pushSTACK(TheSubr(subr_self)->name);
  fehler(type_error,GETTEXT("~: argument ~ should be a nonnegative fixnum"));
}

/* error-message, if an argument is not a Character:
 fehler_char(obj);
 > obj: the erroneous argument
 > subr_self: caller (a SUBR) */
nonreturning_function(global, fehler_char, (object obj)) {
  pushSTACK(obj);          /* TYPE-ERROR slot DATUM */
  pushSTACK(S(character)); /* TYPE-ERROR slot EXPECTED-TYPE */
  pushSTACK(obj); pushSTACK(TheSubr(subr_self)->name);
  fehler(type_error,GETTEXT("~: argument ~ is not a character"));
}

/* error-message, if an argument is not a String:
 > obj: the erroneous argument
 > subr_self: caller (a SUBR) */
nonreturning_function(global, fehler_string, (object obj)) {
  pushSTACK(obj);       /* TYPE-ERROR slot DATUM */
  pushSTACK(S(string)); /* TYPE-ERROR slot EXPECTED-TYPE */
  pushSTACK(obj); pushSTACK(TheSubr(subr_self)->name);
  fehler(type_error,GETTEXT("~: argument ~ is not a string"));
}

/* error-message, if an argument is not a Simple-String:
 > obj: the erroneous argument
 > subr_self: caller (a SUBR) */
nonreturning_function(global, fehler_sstring, (object obj)) {
  pushSTACK(obj);              /* TYPE-ERROR slot DATUM */
  pushSTACK(S(simple_string)); /* TYPE-ERROR slot EXPECTED-TYPE */
  pushSTACK(obj); pushSTACK(TheSubr(subr_self)->name);
  fehler(type_error,GETTEXT("~: argument ~ is not a simple string"));
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
 fehler_string_integer(obj);
 > subr_self: caller (a SUBR) */
nonreturning_function(global, fehler_string_integer, (object obj)) {
  pushSTACK(obj);                    /* TYPE-ERROR slot DATUM */
  pushSTACK(O(type_string_integer)); /* TYPE-ERROR slot EXPECTED-TYPE */
  pushSTACK(obj); pushSTACK(TheSubr(subr_self)->name);
  fehler(type_error,
         GETTEXT("~: argument ~ is neither a string nor an integer"));
}

/* error-message, if an argument is not a Stream:
 fehler_stream(obj);
 > obj: the erroneous argument
 > subr_self: caller (a SUBR) */
nonreturning_function(global, fehler_stream, (object obj)) {
  pushSTACK(obj);       /* TYPE-ERROR slot DATUM */
  pushSTACK(S(stream)); /* TYPE-ERROR slot EXPECTED-TYPE */
  pushSTACK(obj); pushSTACK(TheSubr(subr_self)->name);
  fehler(type_error,GETTEXT("~: argument ~ should be a stream"));
}

/* error-message, if an argument is not a Stream of required stream type:
 fehler_streamtype(obj,type);
 > obj: the erroneous argument
 > type: required stream-type
 > subr_self: caller (a SUBR) */
nonreturning_function(global, fehler_streamtype, (object obj, object type)) {
  pushSTACK(obj);  /* TYPE-ERROR slot DATUM */
  pushSTACK(type); /* TYPE-ERROR slot EXPECTED-TYPE */
  pushSTACK(type); pushSTACK(obj); pushSTACK(TheSubr(subr_self)->name);
  fehler(type_error,GETTEXT("~: argument ~ should be a stream of type ~"));
}

/* error-message, if an argument is not a Function:
 fehler_function(obj);
 > obj: the erroneous argument
 > subr_self: caller (a SUBR) */
nonreturning_function(global, fehler_function, (object obj)) {
  pushSTACK(obj);         /* TYPE-ERROR slot DATUM */
  pushSTACK(S(function)); /* TYPE-ERROR slot EXPECTED-TYPE */
  pushSTACK(obj); pushSTACK(TheSubr(subr_self)->name);
  fehler(type_error,GETTEXT("~: ~ is not a function"));
}

/* signal a type-error when the argument is not a function name */
nonreturning_function(global, fehler_funname_type,
                      (object caller, object obj)) {
  pushSTACK(obj);                         /* TYPE-ERROR slot DATUM */
  pushSTACK(O(type_designator_function)); /* TYPE-ERROR slot EXPECTED-TYPE */
  pushSTACK(obj); pushSTACK(caller);
  fehler(type_error,GETTEXT("~: ~ is not a function name"));
}

/* signal a source-program-error when the argument is not a function name */
nonreturning_function(global, fehler_funname_source,
                      (object caller, object obj)) {
  pushSTACK(obj); pushSTACK(caller);
  fehler(source_program_error,GETTEXT("~: ~ is not a function name"));
}

/* error-message, if an argument is a lambda-expression instead of a function:
 fehler_lambda_expression(obj);
 obj: the erroneous argument
 > subr_self: caller (a SUBR) */
nonreturning_function(global, fehler_lambda_expression, (object obj)) {
  pushSTACK(obj);         /* TYPE-ERROR slot DATUM */
  pushSTACK(S(function)); /* TYPE-ERROR slot EXPECTED-TYPE */
  pushSTACK(obj); pushSTACK(TheSubr(subr_self)->name);
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
 fehler_uint8(obj);
 > obj: erroneous argument
 > subr_self: caller (a SUBR) */
nonreturning_function(global, fehler_uint8, (object obj)) {
  pushSTACK(obj);           /* TYPE-ERROR slot DATUM */
  pushSTACK(O(type_uint8)); /* TYPE-ERROR slot EXPECTED-TYPE */
  pushSTACK(obj); pushSTACK(TheSubr(subr_self)->name);
  fehler(type_error,GETTEXT("~: ~ is not an 8-bit number"));
}

#ifdef HAVE_FFI

/* error, if argument is not an integer of type `sint8' .
 fehler_sint8(obj);
 > obj: erroneous argument
 > subr_self: caller (a SUBR) */
nonreturning_function(global, fehler_sint8, (object obj)) {
  pushSTACK(obj);           /* TYPE-ERROR slot DATUM */
  pushSTACK(O(type_sint8)); /* TYPE-ERROR slot EXPECTED-TYPE */
  pushSTACK(obj); pushSTACK(TheSubr(subr_self)->name);
  fehler(type_error,GETTEXT("~: ~ is not an 8-bit number"));
}

/* error, if argument is not an integer of type `uint16' .
 fehler_uint16(obj);
 > obj: erroneous argument
 > subr_self: caller (a SUBR) */
nonreturning_function(global, fehler_uint16, (object obj)) {
  pushSTACK(obj);            /* TYPE-ERROR slot DATUM */
  pushSTACK(O(type_uint16)); /* TYPE-ERROR slot EXPECTED-TYPE */
  pushSTACK(obj); pushSTACK(TheSubr(subr_self)->name);
  fehler(type_error,GETTEXT("~: ~ is not a 16-bit number"));
}

/* error, if argument is not an integer of type `sint16' .
 fehler_sint16(obj);
 > obj: erroneous argument
 > subr_self: caller (a SUBR) */
nonreturning_function(global, fehler_sint16, (object obj)) {
  pushSTACK(obj);            /* TYPE-ERROR slot DATUM */
  pushSTACK(O(type_sint16)); /* TYPE-ERROR slot EXPECTED-TYPE */
  pushSTACK(obj); pushSTACK(TheSubr(subr_self)->name);
  fehler(type_error,GETTEXT("~: ~ is not a 16-bit number"));
}

/* error, if argument is not an integer of type `uint32' .
 fehler_uint32(obj);
 > obj: erroneous argument
 > subr_self: caller (a SUBR) */
nonreturning_function(global, fehler_uint32, (object obj)) {
  pushSTACK(obj);            /* TYPE-ERROR slot DATUM */
  pushSTACK(O(type_uint32)); /* TYPE-ERROR slot EXPECTED-TYPE */
  pushSTACK(obj); pushSTACK(TheSubr(subr_self)->name);
  fehler(type_error,GETTEXT("~: ~ is not an 32-bit number"));
}

/* error, if argument is not an integer of type `sint32' .
 fehler_sint32(obj);
 > obj: erroneous argument
 > subr_self: caller (a SUBR) */
nonreturning_function(global, fehler_sint32, (object obj)) {
  pushSTACK(obj);            /* TYPE-ERROR slot DATUM */
  pushSTACK(O(type_sint32)); /* TYPE-ERROR slot EXPECTED-TYPE */
  pushSTACK(obj); pushSTACK(TheSubr(subr_self)->name);
  fehler(type_error,GETTEXT("~: ~ is not an 32-bit number"));
}

/* error, if argument is not an integer of type `uint64' .
 fehler_uint64(obj);
 > obj: erroneous argument
 > subr_self: caller (a SUBR) */
nonreturning_function(global, fehler_uint64, (object obj)) {
  pushSTACK(obj);            /* TYPE-ERROR slot DATUM */
  pushSTACK(O(type_uint64)); /* TYPE-ERROR slot EXPECTED-TYPE */
  pushSTACK(obj); pushSTACK(TheSubr(subr_self)->name);
  fehler(type_error,GETTEXT("~: ~ is not an 64-bit number"));
}

/* error, if argument is not an integer of type `sint64' .
 fehler_sint64(obj);
 > obj: erroneous argument
 > subr_self: caller (a SUBR) */
nonreturning_function(global, fehler_sint64, (object obj)) {
  pushSTACK(obj);            /* TYPE-ERROR slot DATUM */
  pushSTACK(O(type_sint64)); /* TYPE-ERROR slot EXPECTED-TYPE */
  pushSTACK(obj); pushSTACK(TheSubr(subr_self)->name);
  fehler(type_error,GETTEXT("~: ~ is not an 64-bit number"));
}

/* error, if argument is not an integer of type `uint' .
 fehler_uint(obj);
 > obj: erroneous argument
 > subr_self: caller (a SUBR) */
nonreturning_function(global, fehler_uint, (object obj)) {
  pushSTACK(obj);            /* TYPE-ERROR slot DATUM */
 #if (int_bitsize==16)
  pushSTACK(O(type_uint16)); /* TYPE-ERROR slot EXPECTED-TYPE */
 #else /* (int_bitsize==32) */
  pushSTACK(O(type_uint32)); /* TYPE-ERROR slot EXPECTED-TYPE */
 #endif
  pushSTACK(obj); pushSTACK(TheSubr(subr_self)->name);
  fehler(type_error,GETTEXT("~: ~ is not an `unsigned int' number"));
}

/* error, if argument is not an integer of type `sint' .
 fehler_sint(obj);
 > obj: erroneous argument
 > subr_self: caller (a SUBR) */
nonreturning_function(global, fehler_sint, (object obj)) {
  pushSTACK(obj);            /* TYPE-ERROR slot DATUM */
 #if (int_bitsize==16)
  pushSTACK(O(type_sint16)); /* TYPE-ERROR slot EXPECTED-TYPE */
 #else /* (int_bitsize==32) */
  pushSTACK(O(type_sint32)); /* TYPE-ERROR slot EXPECTED-TYPE */
 #endif
  pushSTACK(obj); pushSTACK(TheSubr(subr_self)->name);
  fehler(type_error,GETTEXT("~: ~ is not an `int' number"));
}

/* error, if argument is not an integer of type `ulong' .
 fehler_ulong(obj);
 > obj: erroneous argument
 > subr_self: caller (a SUBR) */
nonreturning_function(global, fehler_ulong, (object obj)) {
  pushSTACK(obj);            /* TYPE-ERROR slot DATUM */
 #if (long_bitsize==32)
  pushSTACK(O(type_uint32)); /* TYPE-ERROR slot EXPECTED-TYPE */
 #else /* (long_bitsize==64) */
  pushSTACK(O(type_uint64)); /* TYPE-ERROR slot EXPECTED-TYPE */
 #endif
  pushSTACK(obj); pushSTACK(TheSubr(subr_self)->name);
  fehler(type_error,GETTEXT("~: ~ is not a `unsigned long' number"));
}

/* error, if argument is not an integer of type `slong' .
 fehler_slong(obj);
 > obj: erroneous argument
 > subr_self: caller (a SUBR) */
nonreturning_function(global, fehler_slong, (object obj)) {
  pushSTACK(obj);            /* TYPE-ERROR slot DATUM */
 #if (long_bitsize==32)
  pushSTACK(O(type_sint32)); /* TYPE-ERROR slot EXPECTED-TYPE */
 #else /* (long_bitsize==64) */
  pushSTACK(O(type_sint64)); /* TYPE-ERROR slot EXPECTED-TYPE */
 #endif
  pushSTACK(obj); pushSTACK(TheSubr(subr_self)->name);
  fehler(type_error,GETTEXT("~: ~ is not a `long' number"));
}

/* error, if argument is not a Single-Float.
 fehler_ffloat(obj);
 > obj: erroneous argument
 > subr_self: caller (a SUBR) */
nonreturning_function(global, fehler_ffloat, (object obj)) {
  pushSTACK(obj);             /* TYPE-ERROR slot DATUM */
  pushSTACK(S(single_float)); /* TYPE-ERROR slot EXPECTED-TYPE */
  pushSTACK(obj); pushSTACK(TheSubr(subr_self)->name);
  fehler(type_error,GETTEXT("~: ~ is not a single-float"));
}

/* error, if argument is not a Double-Float.
 fehler_dfloat(obj);
 > obj: erroneous argument
 > subr_self: caller (a SUBR) */
nonreturning_function(global, fehler_dfloat, (object obj)) {
  pushSTACK(obj);             /* TYPE-ERROR slot DATUM */
  pushSTACK(S(double_float)); /* TYPE-ERROR slot EXPECTED-TYPE */
  pushSTACK(obj); pushSTACK(TheSubr(subr_self)->name);
  fehler(type_error,GETTEXT("~: ~ is not a double-float"));
}

#endif
