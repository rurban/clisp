/*
 * Special Forms, Control Structures, Evaluator Related Stuff for CLISP
 * Bruno Haible 1990-2002
 * Sam Steingold 1998-2002
 * German comments translated into English: Stefan Kain 2002-09-28
 */

#include "lispbibl.c"

/* (SYSTEM::%EXIT [errorp]) leaves the system */
LISPFUN(exit,0,1,norest,nokey,0,NIL) {
  var object errorp = STACK_0;
  final_exitcode = missingp(errorp) ? 0 :
                   (posfixnump(errorp) ? posfixnum_to_L(errorp) : 1);
  quit();
}

LISPSPECFORM(eval_when, 1,0,body)
{ /* (EVAL-WHEN ({situation}) {form}), CLTL p. 69 */
  var object situations = STACK_1; /* list of situations */
  /* search symbol EVAL or list (NOT COMPILE) in it: */
  while (consp(situations)) {
    var object situation = Car(situations);
    if (eq(situation,S(eval)) /* symbol EVAL found? */
        || eq(situation,S(Kexecute)))
      goto found;
    if (consp(situation) && eq(Car(situation),S(not))) {
      situation = Cdr(situation);
      if (consp(situation) && nullp(Cdr(situation))
          && (eq(Car(situation),S(compile)) /* list (NOT COMPILE) found? */
              || eq(Car(situation),S(Kcompile_toplevel))))
        goto found;
    }
    situations = Cdr(situations);
  }
  /* symbol EVAL not found */
  VALUES1(NIL);
  skipSTACK(2);
  return;
 found: /* symbol EVAL found */
  var object body = popSTACK();
  skipSTACK(1);
  implicit_progn(body,NIL); /* evaluate body */
}

LISPSPECFORM(quote, 1,0,nobody)
{ /* (QUOTE object) == 'object, CLTL p. 86 */
  VALUES1(popSTACK()); /* argument as value */
}

LISPSPECFORM(function, 1,1,nobody)
{ /* (FUNCTION funname), CLTL. p. 87
 either (FUNCTION symbol)
     or (FUNCTION (LAMBDA . lambdabody))
     or (FUNCTION name (LAMBDA . lambdabody)) */
  var object funname; /* function name (symbol or Lambdabody) */
  var object name; /* name (symbol) */
  if (!boundp(STACK_0)) {
    /* 1 argument */
    funname = STACK_1;
    if (funnamep(funname)) {
      /* (FUNCTION symbol) - syntax
         search symbol in the current function-environment: */
      var object fun = sym_function(funname,aktenv.fun_env);
      /* return SUBR or closure or foreign-function, else error: */
      if (!functionp(fun)) {
        if (functionmacrop(fun))
          fun = TheFunctionMacro(fun)->functionmacro_function;
        else {
          pushSTACK(funname); /* CELL-ERROR Slot NAME */
          pushSTACK(funname);
          pushSTACK(S(function));
          fehler(undefined_function,GETTEXT("~: undefined function ~"));
        }
      }
      VALUES1(fun); skipSTACK(2); return;
    }
    name = S(Klambda); /* :LAMBDA as default name */
  } else {
    /* 2 arguments */
    name = STACK_1; /* first argument */
    if (!funnamep(name))
      fehler_funname_source(S(function),name);
    funname = STACK_0; /* second argument, hopefully lambda expression */
  }
  if (!(consp(funname) && eq(Car(funname),S(lambda)))) /* (LAMBDA . ...) */
    fehler_funname_source(S(function),funname);
  /* lambda expression
     in the current environment, convert into a closure: */
  VALUES1(get_closure(Cdr(funname),name,false,&aktenv));
  skipSTACK(2); return;
}

/* error-message, if a symbol has no value.
 > symbol: symbol
 > subr_self: caller (a SUBR) */
nonreturning_function(local, fehler_no_value, (object symbol)) {
  pushSTACK(symbol); /* CELL-ERROR Slot NAME */
  pushSTACK(symbol);
  pushSTACK(TheSubr(subr_self)->name);
  fehler(unbound_variable,GETTEXT("~: ~ has no dynamic value"));
}

LISPFUNN(psymbol_value,1)
{ /* (SYS::%SYMBOL-VALUE symbol), CLTL p. 90 */
  var object symbol = test_symbol(popSTACK());
  var object val = Symbol_value(symbol);
  if (!boundp(val))
    fehler_no_value(symbol);
  VALUES1(val);
}

LISPFUNN(symbol_value,1)
{ /* (SYMBOL-VALUE symbol), CLTL p. 90 */
  var object symbol = test_symbol(popSTACK());
  var object val = Symbol_value(symbol);
  if (!boundp(val))
    fehler_no_value(symbol);
  if (symbolmacrop(val)) { /* symbol-macro? */
    /* yes -> expand and evaluate: */
    eval_noenv(TheSymbolmacro(val)->symbolmacro_expansion); mv_count=1;
  } else
    VALUES1(val);
}

/* error-message because of undefined function.
 fehler_undef_function(caller,symbol);
 > caller: Caller (a symbol)
 > symbol: symbol or (SETF symbol) */
nonreturning_function(global, fehler_undef_function,
                      (object caller, object symbol)) {
  pushSTACK(symbol); /* CELL-ERROR Slot NAME */
  pushSTACK(symbol);
  pushSTACK(caller);
  fehler(undefined_function,GETTEXT("~: ~ has no global function definition"));
}

LISPFUNN(symbol_function,1)
{ /* (SYMBOL-FUNCTION symbol), CLTL p. 90 */
  var object symbol = test_symbol(popSTACK());
  var object val = Symbol_function(symbol);
  if (!boundp(val))
    fehler_undef_function(S(symbol_function),symbol);
  VALUES1(val);
}

/* UP: just like GET-FUNNAME-SYMBOL (see init.lisp),
   except that it does not create the new symbol when there is none yet
   and does not issue a warning when the SETF symbol is shadowed */
local object funname_to_symbol (object symbol) {
  if (!funnamep(symbol))
    fehler_funname_type(TheSubr(subr_self)->name,symbol);
  if (!symbolp(symbol)) /* (get ... 'SYS::SETF-FUNCTION) */
    symbol = get(Car(Cdr(symbol)),S(setf_function));
  return symbol;
}

LISPFUNN(fdefinition,1)
{ /* (FDEFINITION funname), CLTL2 p. 120 */
  var object funname = popSTACK();
  var object symbol = funname_to_symbol(funname);
  if (!symbolp(symbol)) /* should be a symbol */
      fehler_undef_function(S(fdefinition),funname); /* otherwise undefined */
  var object val = Symbol_function(symbol);
  if (!boundp(val))
    fehler_undef_function(S(fdefinition),funname);
  VALUES1(val);
}

LISPFUNN(boundp,1)
{ /* (BOUNDP symbol), CLTL p. 90 */
  var object symbol = test_symbol(popSTACK());
  VALUES_IF(boundp(Symbol_value(symbol)));
}

LISPFUNN(fboundp,1)
{ /* (FBOUNDP symbol), CLTL p. 90, CLTL2 p. 120 */
  var object symbol = funname_to_symbol(popSTACK());
  VALUES_IF(symbolp(symbol) && /* should be a symbol */
            boundp(Symbol_function(symbol)));
}

LISPFUNN(special_operator_p,1)
{ /* (SPECIAL-OPERATOR-P symbol), was (SPECIAL-FORM-P symbol), CLTL p. 91 */
  var object symbol = test_symbol(popSTACK());
  var object obj = Symbol_function(symbol);
  VALUES_IF(fsubrp(obj));
}

/* UP: Check the body of a SETQ- or PSETQ-form.
 > caller: Caller (a symbol)
 > STACK_0: Body
 < result: true if symbol-macros have to be expanded. */
local bool check_setq_body (object caller) {
  var object body = STACK_0;
  while (consp(body)) {
    var object symbol = test_symbol_non_constant(caller,Car(body));
    if (sym_macrop(symbol))
      return true;
    body = Cdr(body);
    if (atomp(body)) {
      if (!nullp(body))
        goto fehler_dotted;
      /* The whole body is still in STACK_0. */
      pushSTACK(caller);
      fehler(source_program_error,
             GETTEXT("~ called with odd number of arguments: ~"));
    }
    body = Cdr(body);
  }
  /* body is finished. */
  if (!nullp(body)) {
   fehler_dotted: /* The whole body is still in STACK_0. */
    pushSTACK(caller);
    fehler(source_program_error,GETTEXT("dotted list given to ~ : ~"));
  }
  return false;
}

LISPSPECFORM(setq, 0,0,body)
{ /* (SETQ {var form}), CLTL p. 91 */
  if (check_setq_body(S(setq))) {
    var object form = allocate_cons();
    Car(form) = S(setf); Cdr(form) = popSTACK(); /* turn SETQ into SETF */
    eval(form);
  } else {
    var object body = popSTACK();
    if (consp(body)) {
      do {
        var object symbol = Car(body); /* variable */
        body = Cdr(body);
        pushSTACK(Cdr(body)); /* save remaining list */
        pushSTACK(symbol); /* save symbol */
        eval(Car(body)); /* evaluate next form */
        symbol = popSTACK();
        setq(symbol,value1); /* execute assignment */
        body = popSTACK();
      } while (consp(body));
      /* value1 is the last evaluation result. */
    } else {
      value1 = NIL; /* default value at (SETQ) */
    }
    mv_count=1;
  }
}

LISPSPECFORM(psetq, 0,0,body)
{ /* (PSETQ {var form}), CLTL p. 92 */
  if (check_setq_body(S(psetq))) {
    var object form = allocate_cons();
    Car(form) = S(psetf); Cdr(form) = popSTACK(); /* turn PSETQ into PSETF */
    eval(form);
  } else {
    var object body = popSTACK();
    var uintL body_length = llength(body)/2; /* number of pairs (var form) */
    if (body_length > 0) {
      get_space_on_STACK(body_length*2*sizeof(object));
      {
        var uintL count;
        dotimespL(count,body_length, {
          pushSTACK(Car(body)); /* push variable on stack */
          body = Cdr(body);
          pushSTACK(Cdr(body)); /* remaining list on stack */
          eval(Car(body)); /* evaluate next form */
          body = STACK_0;
          STACK_0 = value1; /* its result in the stack */
        });
      }
      {
        var uintL count;
        dotimespL(count,body_length, {
          var object val = popSTACK(); /* value */
          var object sym = popSTACK(); /* symbol */
          setq(sym,val); /* execute assignment */
        });
      }
    }
    VALUES1(NIL);
  }
}

LISPFUNN(set,2)
{ /* (SETF (SYMBOL-VALUE symbol) value) = (SET symbol value), CLTL p. 92 */
  var object symbol = test_symbol_non_constant(S(set),STACK_1);
  if (symbolmacrop(Symbol_value(symbol))) { /* symbol-macro? */
    /* Evaluate `(SETF ,expansion (QUOTE ,value)) */
    pushSTACK(S(setf));
    pushSTACK(TheSymbolmacro(Symbol_value(symbol))->symbolmacro_expansion);
    pushSTACK(S(quote)); pushSTACK(STACK_(0+3)); pushSTACK(listof(2));
    eval_noenv(listof(3)); mv_count=1;
  } else {
    VALUES1(Symbol_value(symbol) = STACK_0);
  }
  skipSTACK(2);
}

LISPFUNN(makunbound,1)
{ /* (MAKUNBOUND symbol), CLTL p. 92 */
  var object symbol = test_symbol_non_constant(S(makunbound),popSTACK());
  Symbol_value(symbol) = unbound;
  VALUES1(symbol);
}

LISPFUNN(fmakunbound,1)
{ /* (FMAKUNBOUND symbol), CLTL p. 92, CLTL2 p. 123 */
  var object funname = popSTACK();
  var object symbol = funname_to_symbol(funname);
  if (!symbolp(symbol)) /* should be a symbol */
      goto undef; /* otherwise undefined */
  {
    var object obj = Symbol_function(symbol);
    if (fsubrp(obj)) {
      pushSTACK(symbol);
      pushSTACK(S(fmakunbound));
      fehler(error,GETTEXT("~: the special operator definition of ~ must not be removed"));
    }
  }
  Symbol_function(symbol) = unbound;
 undef:
  VALUES1(funname);
}

LISPFUN(apply,2,0,rest,nokey,0,NIL)
{ /* (APPLY function {arg} arglist), CLTL p. 107 */
  BEFORE(rest_args_pointer);
  apply(Before(rest_args_pointer), /* function */
        argcount, /* number of {arg} on the stack */
        popSTACK()); /* arglist */
  skipSTACK(1); /* remove function from the stack */
}

LISPFUN(funcall,1,0,rest,nokey,0,NIL)
{ /* (FUNCALL function {arg}), CLTL p. 108 */
  funcall(Before(rest_args_pointer),argcount); skipSTACK(1);
}

LISPSPECFORM(progn, 0,0,body)
{ /* (PROGN {form}), CLTL p. 109 */
  implicit_progn(popSTACK(),NIL);
}

/* Macro: Evaluates the forms of a form list.
 implicit_prog();
 > -(STACK): form list
 increases STACK by 1
 can trigger GC */
#define implicit_prog()                               \
  do { while (mconsp(STACK_0)) {                      \
    var object forms = STACK_0;                       \
    STACK_0 = Cdr(forms);                             \
    eval(Car(forms)); /* evaluate next form */        \
    }                                                 \
    skipSTACK(1);                                     \
  } while(0)

LISPSPECFORM(prog1, 1,0,body)
{ /* (PROG1 form1 {form}), CLTL p. 109 */
  STACK_1 = (eval(STACK_1),value1); /* evaluate form1, save value */
  implicit_prog();
  VALUES1(popSTACK()); /* return saved value */
}

LISPSPECFORM(prog2, 2,0,body)
{ /* (PROG2 form1 form2 {form}), CLTL p. 109 */
  eval(STACK_2); /* evaluate form1 */
  eval(STACK_1); STACK_2 = value1; /* evaluate form2, save value */
  STACK_1 = STACK_0; skipSTACK(1);
  implicit_prog();
  VALUES1(popSTACK()); /* return saved value */
}

/* error-message because of not allowed docstrings
 fehler_docstring(caller,body);
 > caller: Caller, a Symbol
 > body: whole Body */
nonreturning_function(local, fehler_docstring, (object caller, object body)) {
  pushSTACK(body);
  pushSTACK(caller);
  fehler(source_program_error,
         GETTEXT("~: doc-strings are not allowed here: ~"));
}

/* UP for LET, LET*, LOCALLY, MULTIPLE-VALUE-BIND, SYMBOL-MACROLET:
 Compiles the current form and executes it in compiled state.
 compile_form()
 > in STACK: EVAL-frame with the form
 < mv_count/mv_space: Values
 can trigger GC */
local Values compile_eval_form (void)
{ /* execute (SYS::COMPILE-FORM form venv fenv benv genv denv) :
     get the whole form from the EVAL-frame in the stack: */
  pushSTACK(STACK_(frame_form)); /* as first argument */
  {
    var environment_t* stack_env = nest_aktenv(); /* nest current environment, push on STACK */
   #if !defined(STACK_UP)
    var environment_t my_env;
    my_env = *stack_env; /* and transfer here */
    skipSTACK(5); /* and take away from STACK again */
    pushSTACK(my_env.var_env); /* second argument */
    pushSTACK(my_env.fun_env); /* third argument */
    pushSTACK(my_env.block_env); /* fourth argument */
    pushSTACK(my_env.go_env); /* fifth argument */
    pushSTACK(my_env.decl_env); /* sixth argument */
   #endif
    funcall(S(compile_form),6);
  }
  /* call the freshly compiled closure with 0 arguments: */
  funcall(value1,0);
}

/* UP for LET, LET*, LOCALLY, MULTIPLE-VALUE-BIND, SYMBOL-MACROLET:
 Analyzes the variables and declarations, builds up a variable binding-
 frame and extends VENV and poss. also DENV by a frame.
 make_variable_frame(caller,varspecs,&bind_ptr,&bind_count)
 > object caller: Caller, a symbol
 > object varspecs: list of variable-specifiers
 > object value2: list of declaration-specifiers
 > object value1: list ({form}) of forms
 < stack layout: variable binding frame, Env-binding-frame, ({form}).
 < object* bind_ptr: pointer to the first "genuine" binding.
 < uintC bind_count: number of "genuine" bindings.
 changes STACK
 can trigger GC */
local void make_variable_frame (object caller, object varspecs,
                                object** bind_ptr_, uintC* bind_count_)
{
  var object declarations = value2;
  { /* build up variable binding frame: */
    var object* top_of_frame = STACK; /* pointer to frame */
    /* first store the special-declared variables from
       declarations in the stack: */
    var object* spec_pointer = args_end_pointer;
    var uintL spec_anz = 0; /* number of SPECIAL-references */
    {
      var object declspecs = declarations;
      while (consp(declspecs)) {
        var object declspec = Car(declspecs); /* next declaration */
        if (consp(declspec) && eq(Car(declspec),S(special))) { /* (SPECIAL ) */
          while (consp( declspec = Cdr(declspec) )) {
            var object declsym = Car(declspec); /* next special item */
            if (!symbolp(declsym)) { /* should be a symbol */
              pushSTACK(declsym);
              pushSTACK(caller);
              fehler(source_program_error,GETTEXT("~: ~ is not a symbol, but was declared SPECIAL"));
            }
            /* store special-declared symbol in stack: */
            pushSTACK(specdecl); /* SPECDECL as "value" */
            pushSTACK_symbolwithflags(declsym,wbit(active_bit_o)); /* Symbol active */
            check_STACK();
            spec_anz++;
          }
        }
        declspecs = Cdr(declspecs);
      }
    }
    *bind_ptr_ = args_end_pointer; /* pointer to first "genuine" binding */
    { /* Then store the "genuine" variablen bindings (the variable
         and its unevaluated init at a time) in the stack: */
      var uintL var_anz = 0; /* number of variable bindings */
      {
        while (consp(varspecs)) {
          var object varspec = Car(varspecs); /* next varspec */
          /* split up in symbol and init: */
          var object symbol;
          var object init;
          if (symbolp(varspec) && !eq(caller,S(symbol_macrolet))) {
            symbol = varspec; init = unbound;
          } else if /* one-/two-element list, with symbol as CAR ? */
            (consp(varspec)
             && !eq(caller, S(multiple_value_bind))
             && (symbol = Car(varspec), varspec = Cdr(varspec),
                 symbolp(symbol) &&
                 ( /* two elements? */
                  (consp(varspec) && nullp(Cdr(varspec))
                   && (init = Car(varspec), true))
                  || /* one-element (allowed at LET, LET* according to X3J13 vote <182> ) */
                  (nullp(varspec) && !eq(caller,S(symbol_macrolet))
                   && (init = NIL, true))))) {
            /* now init = Car(varspec) or = NIL */
          } else {
            pushSTACK(Car(varspecs));
            pushSTACK(caller);
            fehler(source_program_error,
                   GETTEXT("~: illegal variable specification ~"));
          }
          pushSTACK(init); /* init and */
          pushSTACK_symbolwithflags(symbol,0); /* store variable */
          check_STACK();
          /* determine, if static or dynamic binding: */
          var bool specdecled = false; /* variable is declared special? */
          if (spec_anz > 0) {
           #ifdef NO_symbolflags
            var object* ptr = spec_pointer;
            var uintL count;
            dotimespL(count,spec_anz, {
              NEXT(ptr);
              if (eq(NEXT(ptr),symbol)) {
                if (eq(NEXT(ptr),fixnum(bit(active_bit)))) {
                  specdecled = true; break;
                }
              } else {
                NEXT(ptr);
              }
            });
           #else
            var object to_compare = as_object(as_oint(symbol) | wbit(active_bit_o));
            var object* ptr = spec_pointer;
            var uintL count;
            dotimespL(count,spec_anz, {
              NEXT(ptr);
              if (eq(NEXT(ptr),to_compare)) {
                specdecled = true; break;
              }
            });
           #endif
          }
          if (eq(caller,S(symbol_macrolet))) {
            if (constantp(TheSymbol(symbol))
                || special_var_p(TheSymbol(symbol))) {
              pushSTACK(symbol);
              pushSTACK(caller);
              fehler(program_error,
                     GETTEXT("~: symbol ~ is declared special and must not be declared a macro"));
            }
            if (specdecled) {
              pushSTACK(symbol);
              pushSTACK(caller);
              fehler(source_program_error,
                     GETTEXT("~: symbol ~ must not be declared SPECIAL and a macro at the same time"));
            }
            /* static binding */
          } else {
            if (constantp(TheSymbol(symbol))) {
              pushSTACK(symbol);
              pushSTACK(caller);
              fehler(program_error,
                     GETTEXT("~: ~ is a constant, cannot be bound"));
            }
            if (specdecled || special_var_p(TheSymbol(symbol))) {
              /* bind dynamically */
              STACK_0 = as_object(as_oint(STACK_0) | wbit(dynam_bit_o));
            } else {
              /* bind statically */
            }
          }
          varspecs = Cdr(varspecs);
          var_anz++;
        }
      }
      *bind_count_ = var_anz;
      var_anz += spec_anz; /* total number of symbol/value pairs */
     #ifndef UNIX_DEC_ULTRIX_GCCBUG
      if (var_anz > (uintC)(~(uintC)0)) { /* does it fit into a uintC ? */
        pushSTACK(caller);
        fehler(source_program_error,
               GETTEXT("~: too many variables and/or declarations"));
      }
     #endif
      pushSTACK(aktenv.var_env); /* current VAR_ENV as NEXT_ENV */
      pushSTACK(as_object(var_anz)); /* number of bindings */
      finish_frame(VAR);
    }
  }
  /* The variable binding frame is now finished. */
  var object* var_frame_ptr = STACK; /* pointer to variable binding frame */
  { /* build up VENV binding frame: */
    var object* top_of_frame = STACK; /* pointer to frame */
    /* first extend DENV by the necessary declspecs: */
    var object denv = aktenv.decl_env;
    pushSTACK(value1); /* save ({form}) */
    pushSTACK(declarations);
    while (mconsp(STACK_0)) {
      var object declspecs = STACK_0;
      STACK_0 = Cdr(declspecs);
      var object declspec = Car(declspecs); /* next Declspec */
      if (consp(declspec)) { /* should be a cons */
        if (!eq(Car(declspec),S(special))) /* we have treated (SPECIAL ...) already */
          denv = augment_decl_env(declspec,denv); /* process everything else */
      }
    }
    skipSTACK(1);
    var object forms = popSTACK();
    /* now build the frame: */
    if (eq(denv,aktenv.decl_env)) {
      pushSTACK(aktenv.var_env);
      finish_frame(ENV1V);
    } else {
      pushSTACK(aktenv.decl_env);
      pushSTACK(aktenv.var_env);
      finish_frame(ENV2VD);
      aktenv.decl_env = denv;
    }
    /* VENV-binding frame is finished. */
    aktenv.var_env = make_framepointer(var_frame_ptr); /* pointer to variable binding frame */
    pushSTACK(forms);
  }
}

LISPSPECFORM(let, 1,0,body)
{ /* (LET ({varspec}) {decl} {form}), CLTL p. 110 */
  /* separate {decl} {form}: */
  var bool to_compile = parse_dd(STACK_0,aktenv.var_env,aktenv.fun_env); /* incomplete var_env?? */
  /* no docstring, please: */
  if (!nullp(value3))
    fehler_docstring(S(let),STACK_0);
  if (to_compile) { /* declaration (COMPILE) ? */
    /* yes -> compile form: */
    skipSTACK(2); return_Values compile_eval_form();
  } else {
    skipSTACK(1);
    /* build variable binding frame, extend VAR_ENV : */
    var object* bind_ptr;
    var uintC bind_count;
    make_variable_frame(S(let),popSTACK(),&bind_ptr,&bind_count);
    if (bind_count > 0) {
      { /* Then, evaluate the initialization forms: */
        var object* frame_pointer = bind_ptr;
        var uintC count;
        dotimespC(count,bind_count, {
          var object* initptr = &NEXT(frame_pointer);
          var object init = *initptr; /* next init */
          *initptr = (!boundp(init) ? NIL : (eval(init),value1)); /* evaluate, NIL as default */
          frame_pointer skipSTACKop -(varframe_binding_size-1);
        });
      }
      { /* Then, activate the bindings: */
        var object* frame_pointer = bind_ptr;
        var uintC count;
        dotimespC(count,bind_count, {
          frame_pointer skipSTACKop -varframe_binding_size;
          var object* markptr = &Before(frame_pointer);
          if (as_oint(*markptr) & wbit(dynam_bit_o)) { /* binding dynamic? */
            var object symbol = *(markptr STACKop varframe_binding_sym); /* variable */
            var object newval = *(markptr STACKop varframe_binding_value); /* new value */
            *(markptr STACKop varframe_binding_value) = TheSymbolflagged(symbol)->symvalue; /* save old value in frame */
            *markptr = as_object(as_oint(*markptr) | wbit(active_bit_o)); /* active binding */
            TheSymbolflagged(symbol)->symvalue = newval; /* new value */
          } else {
            *markptr = as_object(as_oint(*markptr) | wbit(active_bit_o)); /* activate binding */
          }
        });
      }
    }
    /* interpret body: */
    implicit_progn(popSTACK(),NIL);
    /* unwind frames: */
    unwind(); /* unwind VENV binding frame */
    unwind(); /* unwind variable binding frame */
  }
}

LISPSPECFORM(letstern, 1,0,body)
{ /* (LET* ({varspec}) {decl} {form}), CLTL p. 111 */
  /* separate {decl} {form} : */
  var bool to_compile = parse_dd(STACK_0,aktenv.var_env,aktenv.fun_env); /* incomplete var_env?? */
  /* no docstring, please: */
  if (!nullp(value3))
    fehler_docstring(S(letstern),STACK_0);
  if (to_compile) { /* declaration (COMPILE) ? */
    /* yes -> compile form: */
    skipSTACK(2); return_Values compile_eval_form();
  } else {
    skipSTACK(1);
    /* build variable binding frame, extend VAR_ENV : */
    var object* bind_ptr;
    var uintC bind_count;
    make_variable_frame(S(letstern),popSTACK(),&bind_ptr,&bind_count);
    /* Then, evaluate the initialization forms and activate the bindings */
    if (bind_count > 0) {
      var object* frame_pointer = bind_ptr;
      var uintC count;
      dotimespC(count,bind_count, {
        var object* initptr = &Next(frame_pointer);
        frame_pointer skipSTACKop -varframe_binding_size;
        var object* markptr = &Before(frame_pointer);
        var object init = *initptr; /* next init */
        var object newval = (!boundp(init) ? NIL : (eval(init),value1)); /* evaluate, NIL as default */
        if (as_oint(*markptr) & wbit(dynam_bit_o)) { /* binding dynamic? */
          var object symbol = *(markptr STACKop varframe_binding_sym); /* variable */
          *initptr = TheSymbolflagged(symbol)->symvalue; /* save old value in frame */
          *markptr = as_object(as_oint(*markptr) | wbit(active_bit_o)); /* activate binding */
          TheSymbolflagged(symbol)->symvalue = newval; /* new value */
        } else {
          *initptr = newval; /* new value into the frame */
          *markptr = as_object(as_oint(*markptr) | wbit(active_bit_o)); /* activate binding */
        }
      });
    }
    /* interpret body: */
    implicit_progn(popSTACK(),NIL);
    /* unwind frames: */
    unwind(); /* unwind VENV-binding frame */
    unwind(); /* unwind variable binding frame */
  }
}

LISPSPECFORM(locally, 0,0,body)
{ /* (LOCALLY {decl} {form}), CLTL2 p. 221 */
  /* separate {decl} {form} : */
  var bool to_compile = parse_dd(STACK_0,aktenv.var_env,aktenv.fun_env); /* incomplete var_env?? */
  /* please no docstring: */
  if (!nullp(value3))
    fehler_docstring(S(locally),STACK_0);
  skipSTACK(1);
  if (to_compile) { /* declaration (COMPILE) ? */
    /* yes -> compile form: */
    return_Values compile_eval_form();
  } else {
    /* build variable binding frame, extend VAR_ENV : */
    var object* bind_ptr;
    var uintC bind_count;
    make_variable_frame(S(locally),NIL,&bind_ptr,&bind_count);
    /* interpret body: */
    implicit_progn(popSTACK(),NIL);
    /* unwind frames: */
    unwind(); /* unwind VENV-binding frame */
    unwind(); /* unwind variable binding frame */
  }
}

LISPSPECFORM(compiler_let, 1,0,body)
{ /* (COMPILER-LET ({varspec}) {form}), CLTL p. 112 */
  var object* varspecs_ = &STACK_1;
  var object varspecs = *varspecs_; /* list of variables */
  var uintL varcount = llength(varspecs); /* number of variables */
  get_space_on_STACK(varcount*3*sizeof(object));
  /* evaluate varspecs: */
  var object* val_pointer = args_end_pointer; /* pointer to values */
  while (consp(varspecs)) {
    var object varspec = Car(varspecs);
    var object symbol;
    if (consp(varspec)) {
      /* varspec is a Cons */
      symbol = test_symbol_non_constant(S(compiler_let),Car(varspec));
      varspec = Cdr(varspec);
      if (consp(varspec) && nullp(Cdr(varspec))) {
        varspec = Car(varspec); /* Initform = second list element */
      } else if (nullp(varspec)) { /* allowed by X3J13 vote <182> */
        /* varspec = NIL; */ /* Initform = NIL */
      } else {
        pushSTACK(Car(varspecs));
        pushSTACK(S(compiler_let));
        fehler(source_program_error,
               GETTEXT("~: illegal variable specification ~"));
      }
      pushSTACK(Cdr(varspecs));
      eval_noenv(varspec); /* evaluate initform */
      varspecs = STACK_0;
      STACK_0 = value1; /* and into the stack */
    } else {
      symbol = test_symbol_non_constant(S(compiler_let),varspec);
      pushSTACK(NIL); /* NIL as value into the stack */
      varspecs = Cdr(varspecs);
    }
  }
  varspecs = *varspecs_;
  { /* build Frame: */
    var object* top_of_frame = STACK; /* pointer to frame */
    while (consp(varspecs)) {
      var object varspec = Car(varspecs);
      if (consp(varspec))
          varspec = Car(varspec);
      pushSTACK(Symbol_value(varspec)); /* old value of the variables */
      pushSTACK(varspec); /* variable */
      varspecs = Cdr(varspecs);
    }
    finish_frame(DYNBIND);
  }
  /* frame finished, now change the values of the variables: */
  varspecs = *varspecs_;
  {
    var object* valptr = val_pointer;
    while (consp(varspecs)) {
      var object varspec = Car(varspecs);
      if (consp(varspec))
        varspec = Car(varspec);
      Symbol_value(varspec) = NEXT(valptr); /* assign new value to the variables */
        varspecs = Cdr(varspecs);
    }
  }
  /* now evaluate the forms: */
  implicit_progn(*(varspecs_ STACKop -1),NIL);
  /* unwind binding frame: */
  unwind();
  /* clean up stack: */
  set_args_end_pointer(val_pointer);
  skipSTACK(2);
}

LISPSPECFORM(progv, 2,0,body)
{ /* (PROGV symbollist valuelist {form}), CLTL p. 112 */
  STACK_2 = (eval(STACK_2),value1); /* evaluate symbol list */
  var object valuelist = (eval(STACK_1),value1); /* evaluate value list */
  var object body = popSTACK();
  skipSTACK(1);
  progv(popSTACK(),valuelist); /* build frame */
  implicit_progn(body,NIL); /* evaluate body */
  unwind(); /* unwind frame */
}

/* error-message at FLET/LABELS, if there is no function specification.
 > caller: Caller, a symbol
 > obj: erroneous function specification */
nonreturning_function(local, fehler_funspec, (object caller, object obj)) {
  pushSTACK(obj);
  pushSTACK(caller);
  fehler(source_program_error,GETTEXT("~: ~ is not a function specification"));
}

/* skip all declarations from the body:
 descructively modifies BODY to remove (DECLARE ...)
 statements from its beginning */
local void skip_declarations (object* body) {
  while (consp(*body) && consp(Car(*body)) && eq(S(declare),Car(Car(*body))))
    *body = Cdr(*body);
}

/* UP: Finishes a FLET/MACROLET.
 finish_flet(top_of_frame,body);
 > stack layout: [top_of_frame] def1 name1 ... defn namen [STACK]
 > top_of_frame: pointer to frame
 > body: list of forms
 < mv_count/mv_space: values
 can trigger GC */
local Values finish_flet (object* top_of_frame, object body) {
  {
    var uintL bindcount = /* number of bindings */
      STACK_item_count(STACK,top_of_frame) / 2;
      pushSTACK(aktenv.fun_env); /* current FUN_ENV as NEXT_ENV */
    pushSTACK(as_object(bindcount));
    finish_frame(FUN);
  }
  /* function binding frame is finished.
     build FENV-binding frame: */
  {
    var object* top_of_frame = STACK; /* pointer to frame */
    pushSTACK(aktenv.fun_env);
    finish_frame(ENV1F);
    /* FENV-binding frame is finished.
       extend FUN_ENV:
       top_of_frame = pointer to the function binding frame */
    aktenv.fun_env = make_framepointer(top_of_frame);
  }
  /* allow declarations, as per ANSI CL */
  skip_declarations(&body);
  /* execute forms: */
  implicit_progn(body,NIL);
  unwind(); /* unwind FENV binding frame */
  unwind(); /* unwind function binding frame */
}

LISPSPECFORM(flet, 1,0,body)
{ /* (FLET ({funspec}) {form}), CLTL p. 113 */
  var object body = popSTACK(); /* ({form}) */
  var object funspecs = popSTACK(); /* ({funspec}) */
  /* build function binding frame: */
  var object* top_of_frame = STACK; /* pointer to frame */
  while (consp(funspecs)) {
    pushSTACK(body); /* save form list */
    pushSTACK(Cdr(funspecs)); /* remaining funspecs */
    funspecs = Car(funspecs); /* next funspec = (name . lambdabody) */
    /* should be a cons, whose CAR is a symbol and whose CDR is a cons: */
    if (!consp(funspecs)) {
     fehler_spec:
      fehler_funspec(S(flet),funspecs);
    }
    var object name = Car(funspecs);
    var object lambdabody = Cdr(funspecs);
    if (!funnamep(name))
      fehler_funname_source(S(flet),name);
    if (!consp(lambdabody))
      goto fehler_spec;
    pushSTACK(name); /* save name */
    /* turn lambdabody into a closure: */
    var object fun = get_closure(lambdabody,name,true,&aktenv);
    name = popSTACK();
    funspecs = popSTACK(); /* remaining funspecs */
    body = popSTACK();
    /* into the frame: */
    pushSTACK(fun); /* as "value" the closure */
    pushSTACK(name); /* name, binding is automatically active */
  }
  return_Values finish_flet(top_of_frame,body);
}

LISPSPECFORM(labels, 1,0,body)
{ /* (LABELS ({funspec}) {form}), CLTL p. 113 */
  /* We can dispense with the construction of a function binding frame,
     because when creating the first closure, the environment is nested anyway
     and thereby this function binding frame would be written into a vector.
     nest the current FUN_ENV: */
  pushSTACK(nest_fun(aktenv.fun_env));
  /* determine the number of funspecs and test the syntax: */
  var uintL veclength = 1; /* = 2 * (number of funspecs) + 1 */
  {
    var object funspecs = STACK_(1+1);
    while (consp(funspecs)) {
      var object funspec = Car(funspecs);
      /* should be a cons, whose CAR is a symbol and whose CDR is a cons: */
      if (!consp(funspec)) {
       fehler_spec:
        fehler_funspec(S(labels),funspec);
      }
      var object name = Car(funspec);
      var object lambdabody = Cdr(funspec);
      if (!funnamep(name))
        fehler_funname_source(S(labels),name);
      if (!consp(lambdabody))
        goto fehler_spec;
      funspecs = Cdr(funspecs);
      veclength += 2;
    }
  }
  /* allocate vector of suitable length and store the names: */
  var object vec = allocate_vector(veclength);
  {
    var object* ptr = &TheSvector(vec)->data[0];
    var object funspecs = STACK_(1+1);
    while (consp(funspecs)) {
      *ptr++ = Car(Car(funspecs)); /* next name */
      ptr++; /* function remains NIL for the time being */
      funspecs = Cdr(funspecs);
    }
    *ptr++ = popSTACK(); /* nested FUN_ENV as last vector-element */
  }
  var object body = popSTACK(); /* form list */
  var object funspecs = popSTACK();
  { /* build FENV binding frame: */
    var object* top_of_frame = STACK; /* pointer to frame */
    pushSTACK(aktenv.fun_env);
    finish_frame(ENV1F);
  }
  /* extend FUN_ENV: */
  aktenv.fun_env = vec;
  /* create closures and put into the vector: */
  pushSTACK(body);
  pushSTACK(vec);
  {
    var uintL index = 1; /* index into the vector */
    while (consp(funspecs)) {
      pushSTACK(Cdr(funspecs)); /* remaining funspecs */
      var object funspec = Car(funspecs);
      /* create closure: */
      var object fun = get_closure(Cdr(funspec),Car(funspec),true,&aktenv);
      funspecs = popSTACK();
      TheSvector(STACK_0)->data[index] = fun; /* put into the vector */
      index += 2;
    }
  }
  skipSTACK(1); /* forget vector */
  body = popSTACK();
  /* allow declarations, as per ANSI CL */
  skip_declarations(&body);
  /* execute forms: */
  implicit_progn(body,NIL);
  unwind(); /* unwind FENV binding frame */
}

LISPSPECFORM(macrolet, 1,0,body)
{ /* (MACROLET ({macrodef}) {form}), CLTL p. 113 */
  var object body = popSTACK(); /* ({form}) */
  var object macrodefs = popSTACK(); /* ({macrodef}) */
  /* build macrobinding frame: */
  var object* top_of_frame = STACK; /* pointer to frame */
  while (consp(macrodefs)) {
    pushSTACK(body); /* save form list */
    pushSTACK(Cdr(macrodefs)); /* remaining macrodefs */
    macrodefs = Car(macrodefs); /* next macrodef = (name . lambdabody) */
    /* should be a cons, whose CAR is a symbol and whose CDR is a cons: */
    if (!consp(macrodefs)) {
     fehler_spec:
      pushSTACK(macrodefs);
      pushSTACK(S(macrolet));
      fehler(source_program_error,
             GETTEXT("~: ~ is not a macro specification"));
    }
    var object name = Car(macrodefs);
    if (!symbolp(name)) {
      pushSTACK(name);
      pushSTACK(S(macrolet));
      fehler(source_program_error,
             GETTEXT("~: macro name ~ should be a symbol"));
    }
    if (!mconsp(Cdr(macrodefs)))
      goto fehler_spec;
    pushSTACK(name); /* save name */
    /* build macro-expander: (SYSTEM::MAKE-MACRO-EXPANDER macrodef) */
    pushSTACK(macrodefs); funcall(S(make_macro_expander),1);
    name = popSTACK();
    macrodefs = popSTACK(); /* remaining macrodefs */
    body = popSTACK();
    /* into the frame: */
    pushSTACK(value1); /* as "value" the cons with the expander */
    pushSTACK(name); /* name, binding is automatically active */
  }
  return_Values finish_flet(top_of_frame,body);
}

LISPSPECFORM(function_macro_let, 1,0,body)
{ /* (SYSTEM::FUNCTION-MACRO-LET ({(name fun-lambdabody macro-lambdabody)})
        {form})
 is similar to FLET, except that alternative macro definitions
 are provided for every function. */
  var object body = popSTACK(); /* ({form}) */
  var object funmacspecs = popSTACK(); /* {(name fun-lambdabody macro-lambdabody)} */
  /* build FunctionMacro bindings frame : */
  var object* top_of_frame = STACK; /* pointer to frame */
  while (consp(funmacspecs)) {
    pushSTACK(body); /* save form list */
    pushSTACK(Cdr(funmacspecs)); /* remaining funmacspecs */
    funmacspecs = Car(funmacspecs);
    /* next (name fun-lambdabody macro-lambdabody) should be
       a list of length 3, whose CAR is a symbol
       and whose further list elements are conses: */
    if (!consp(funmacspecs)) {
     fehler_spec:
      pushSTACK(funmacspecs);
      pushSTACK(S(function_macro_let));
      fehler(source_program_error,
             GETTEXT("~: ~ is not a function and macro specification"));
    }
    var object name = Car(funmacspecs);
    if (!symbolp(name)) {
      pushSTACK(name);
      pushSTACK(S(function_macro_let));
      fehler(source_program_error,
             GETTEXT("~: function and macro name ~ should be a symbol"));
    }
    if (!(mconsp(Cdr(funmacspecs)) && mconsp(Car(Cdr(funmacspecs)))
          && mconsp(Cdr(Cdr(funmacspecs)))
          && mconsp(Car(Cdr(Cdr(funmacspecs))))
          && nullp(Cdr(Cdr(Cdr(funmacspecs))))))
      goto fehler_spec;
    pushSTACK(name); /* save name */
    pushSTACK(Car(Cdr(funmacspecs))); /* fun-lambdabody */
    pushSTACK(Car(Cdr(Cdr(funmacspecs)))); /* macro-lambdabody */
    /* turn fun-lambdabody into a closure: */
    STACK_1 = get_closure(STACK_1,name,false,&aktenv);
    { /* build macro-expander:
         (SYSTEM::MAKE-MACRO-EXPANDER (cons name macro-lambdabody)) */
      var object macrodef = allocate_cons();
      Car(macrodef) = STACK_2; Cdr(macrodef) = STACK_0;
      pushSTACK(macrodef); funcall(S(make_macro_expander),1);
      pushSTACK(value1); C_macro_expander();
        STACK_0 = value1;
    }
    /* collect both: */
    C_make_function_macro();
    name = popSTACK();
    funmacspecs = popSTACK(); /* remaining funmacspecs */
    body = popSTACK();
    /* into the Frame: */
    pushSTACK(value1); /* as "value" the FunctionMacro */
    pushSTACK(name); /* name, binding is automatically active */
  }
  return_Values finish_flet(top_of_frame,body);
}

LISPSPECFORM(symbol_macrolet, 1,0,body)
{ /* (SYMBOL-MACROLET ({(var expansion)}) {decl} {form}), CLTL2 p. 155 */
  /* separate {decl} {form} : */
  var bool to_compile = parse_dd(STACK_0,aktenv.var_env,aktenv.fun_env); /* incomplete var_env?? */
  /* please no docstring: */
  if (!nullp(value3))
    fehler_docstring(S(symbol_macrolet),STACK_0);
  if (to_compile) { /* declaration (COMPILE) ? */
    /* yes -> compile form: */
    skipSTACK(2); return_Values compile_eval_form();
  } else {
    skipSTACK(1);
    /* build variable binding frame, extend VAR_ENV : */
    var object* bind_ptr;
    var uintC bind_count;
    make_variable_frame(S(symbol_macrolet),popSTACK(),&bind_ptr,&bind_count);
    /* then form the symbol-macros and activate the bindings: */
    if (bind_count > 0) {
      var object* frame_pointer = bind_ptr;
      var uintC count;
      dotimespC(count,bind_count, {
        var object* initptr = &NEXT(frame_pointer);
        var object sm = allocate_symbolmacro();
        TheSymbolmacro(sm)->symbolmacro_expansion = *initptr;
        *initptr = sm;
        frame_pointer skipSTACKop -(varframe_binding_size-1);
        Before(frame_pointer) = as_object(as_oint(Before(frame_pointer)) | wbit(active_bit_o));
      });
    }
    /* interpret body: */
    implicit_progn(popSTACK(),NIL);
    /* unwind frames: */
    unwind(); /* unwind VENV-binding frame */
    unwind(); /* unwind variable-binding-frame */
  }
}

LISPSPECFORM(if, 2,1,nobody)
{ /* (IF test form1 [form2]), CLTL p. 115 */
  eval(STACK_2); /* evaluate condition */
  var object form;
  if (!nullp(value1)) {
    form = STACK_1; skipSTACK(3); /* evaluate form1 */
  } else {
    form = STACK_0; skipSTACK(3); /* evaluate form2 */
    if (!boundp(form)) {
      VALUES1(NIL); return; /* not supplied -> NIL */
    }
  }
  eval(form);
}

LISPSPECFORM(when, 1,0,body)
{ /* (WHEN test {form}), CLTL p. 115 */
  eval(STACK_1); /* evaluate condition */
  if (!nullp(value1)) {
    var object body = STACK_0;
    skipSTACK(2);
    implicit_progn(body,NIL);
  } else {
    skipSTACK(2);
    VALUES1(NIL);
  }
}

LISPSPECFORM(unless, 1,0,body)
{ /* (UNLESS test {form}), CLTL p. 115 */
  eval(STACK_1); /* evaluate condition */
  if (nullp(value1)) {
    var object body = STACK_0;
    skipSTACK(2);
    implicit_progn(body,NIL);
  } else {
    skipSTACK(2);
    VALUES1(NIL);
  }
}

LISPSPECFORM(cond, 0,0,body)
{ /* (COND {(bed {form})}), CLTL p. 116 */
  while (mconsp(STACK_0)) {
    var object clause = STACK_0; /* clause-list */
    STACK_0 = Cdr(clause); /* save remaining clauses */
    clause = Car(clause); /* next clause */
    if (!consp(clause)) { /* should be a cons */
      pushSTACK(clause);
      pushSTACK(S(cond));
      fehler(source_program_error,GETTEXT("~: clause ~ should be a list"));
    }
    pushSTACK(Cdr(clause)); /* save clause rest */
    eval(Car(clause)); /* evaluate condition */
    if (!nullp(value1))
      goto eval_clause;
    skipSTACK(1); /* try next */
  }
  /* no condition was fulfilled. */
  VALUES1(NIL); skipSTACK(1); return;
  /* fulfilled condition found: */
 eval_clause:
  var object clause_rest = popSTACK(); /* clause rest */
  skipSTACK(1);
  implicit_progn(clause_rest,value1); /* evaluate */
}

LISPSPECFORM(case, 1,0,body)
{ /* (CASE keyform {(keys {form})}), CLTL p. 117 */
  eval(STACK_1); /* evaluate keyform */
  var object value = value1;
  var object clauses = STACK_0;
  var object clause;
  skipSTACK(2);
  while (consp(clauses)) {
    clause = Car(clauses); /* next clause */
    clauses = Cdr(clauses);
    if (!consp(clause)) { /* should be a cons */
      pushSTACK(clause);
      pushSTACK(S(case));
      fehler(source_program_error,GETTEXT("~: missing key list: ~"));
    }
    var object keys = Car(clause);
    if (eq(keys,T) || eq(keys,S(otherwise))) {
      if (nullp(clauses))
        goto eval_clause;
      pushSTACK(keys);
      pushSTACK(S(case));
      fehler(source_program_error,
             GETTEXT("~: the ~ clause must be the last one"));
    } else {
      if (listp(keys)) {
        while (consp(keys)) {
          if (eql(Car(keys),value))
            goto eval_clause;
          keys = Cdr(keys);
        }
      } else {
        if (eql(keys,value))
          goto eval_clause;
      }
    }
  }
  /* no condition was fulfilled. */
  VALUES1(NIL); return;
  /* fulfilled condition found: */
 eval_clause:
  var object clause_rest = Cdr(clause); /* clause-rest */
  implicit_progn(clause_rest,NIL); /* evaluate */
}

LISPSPECFORM(block, 1,0,body)
{ /* (BLOCK name {form}), CLTL p. 119 */
  var object body = popSTACK();
  var object name = test_symbol(popSTACK());
  var sp_jmp_buf returner; /* return point */
  { /* build block-frame: */
    var object* top_of_frame = STACK; /* pointer to frame */
    pushSTACK(name); /* block-name */
    pushSTACK(aktenv.block_env); /* current BLOCK_ENV as NEXT_ENV */
    finish_entry_frame(IBLOCK,&!returner,, goto block_return; );
  }
  { /* build BENV-frame: */
    var object* top_of_frame = STACK;
    pushSTACK(aktenv.block_env);
    finish_frame(ENV1B);
    /* extend BLOCK_ENV (top_of_frame = pointer to the block-frame) */
    aktenv.block_env = make_framepointer(top_of_frame);
  }
  /* execute body: */
  implicit_progn(body,NIL);
  unwind(); /* unwind BENV-binding frame */
 block_return: /* we jump to this label, if the BLOCK-Frame
                  has caught a RETURN-FROM. */
  unwind(); /* unwind BLOCK-frame */
}

/* error-message, if a block has already been left.
 fehler_block_left(name);
 > name: block-name */
nonreturning_function(global, fehler_block_left, (object name)) {
  pushSTACK(name);
  pushSTACK(S(return_from));
  fehler(control_error,
         GETTEXT("~: the block named ~ has already been left"));
}

LISPSPECFORM(return_from, 1,1,nobody)
{ /* (RETURN-FROM name [result]), CLTL p. 120 */
  var object name = test_symbol(STACK_1);
  /* traverse BLOCK_ENV: */
  var object env = aktenv.block_env; /* current BLOCK_ENV */
  var object* FRAME;
  while (framepointerp(env)) {
    /* env is a frame-pointer to a IBLOCK-frame in the stack. */
    FRAME = TheFramepointer(env);
    if (framecode(FRAME_(0)) & bit(nested_bit_t)) {
      /* frame already nested */
      env = FRAME_(frame_next_env); break;
    }
    if (eq(FRAME_(frame_name),name))
      goto found;
    env = FRAME_(frame_next_env);
  }
  /* env is an Alist. */
  while (consp(env)) {
    var object block_cons = Car(env);
    if (eq(Car(block_cons),name)) {
      env = Cdr(block_cons);
      if (eq(env,disabled)) /* block still active? */
        fehler_block_left(name);
      goto found;
      }
    env = Cdr(env);
  }
  /* env is done. */
  pushSTACK(name);
  pushSTACK(S(return_from));
  fehler(source_program_error,
         GETTEXT("~: no block named ~ is currently visible"));
  /* found block-frame: env */
 found:
  FRAME = uTheFramepointer(env); /* pointer to that frame */
  /* produce values, with which the block will be left: */
  var object result = popSTACK();
  skipSTACK(1);
  if (boundp(result)) { /* result supplied? */
    eval(result);
  } else {
      VALUES1(NIL);
  }
  /* jump to the found block-frame and unwind it: */
  unwind_upto(FRAME);
}

/* UP: check whenter OBJ ends a proper list */
#define ENDP(caller,obj) (consp(obj) ? false : nullp(obj) ? true : \
                          (fehler_proper_list(caller,obj), 0))

/* We build the functions MAPCAR, MAPLIST, MAPCAN, MAPCON in two versions:
 The first one builds the list in reversed order, then has to reverse it.
 The second one works in forward direction, but needs one cons too much. */
#define MAP_REVERSES

#ifdef MAP_REVERSES

/* macro for MAPCAR and MAPLIST */
#define MAPCAR_MAPLIST_BODY(listaccess,caller)                          \
  { var object* args_pointer = rest_args_pointer STACKop 2;             \
    argcount++; /* argcount := number of lists on the stack */          \
    /* reserve space for the function call arguments: */                \
    get_space_on_STACK(sizeof(object)*(uintL)argcount);                 \
    pushSTACK(NIL); /* start of the result list */                      \
   {var object* ergptr = &STACK_0; /* pointer to it */                  \
    /* traverse all lists in parallel: */                               \
    loop { var object* argptr = args_pointer;                           \
      var object fun = NEXT(argptr);                                    \
      var uintC count;                                                  \
      dotimespC(count,argcount,{                                        \
        var object* next_list_ = &NEXT(argptr);                         \
        var object next_list = *next_list_;                             \
        if (ENDP(caller,next_list)) goto fertig; /* a list ended -> done */ \
        pushSTACK(listaccess(next_list)); /* as argument on the stack */ \
        *next_list_ = Cdr(next_list); /* shorten list */                \
      });                                                               \
      funcall(fun,argcount); /* call function */                        \
      pushSTACK(value1);                                                \
     {var object new_cons = allocate_cons(); /* new cons */             \
      Car(new_cons) = popSTACK(); Cdr(new_cons) = STACK_0;              \
      STACK_0 = new_cons; /* lengthen the result list */                \
     }}                                                                 \
    fertig:                                                             \
    VALUES1(nreverse(*ergptr)); /* reverse result list */               \
    set_args_end_pointer(args_pointer); /* clean up STACK */            \
   }}

#else

/* macro for MAPCAR and MAPLIST */
#define MAPCAR_MAPLIST_BODY(listaccess,caller)                          \
  { var object* args_pointer = rest_args_pointer STACKop 2;             \
    argcount++; /* argcount := number of lists on the stack */          \
    /* reserve space for the function call arguments: */                \
    get_space_on_STACK(sizeof(object)*(uintL)argcount);                 \
    /* start total list: */                                             \
   {var object new_cons = allocate_cons(); /* (CONS NIL NIL) */         \
    pushSTACK(new_cons); /* total list */                               \
    pushSTACK(new_cons); /* (last totallist) */                         \
   }                                                                    \
  {var object* ergptr = &STACK_1; /* pointer to it */                   \
   /* traverse all lists in parallel: */                                \
   loop { var object* argptr = args_pointer;                            \
     var object fun = NEXT(argptr);                                     \
     var uintC count;                                                   \
     dotimespC(count,argcount,{                                         \
       var object* next_list_ = &NEXT(argptr);                          \
       var object next_list = *next_list_;                              \
       if (ENDP(caller,next_list)) goto fertig; /* a list ended -> done */ \
       pushSTACK(listaccess(next_list)); /* as argument on the stack */ \
       *next_list_ = Cdr(next_list); /* shorten list */                 \
     });                                                                \
     funcall(fun,argcount); /* call function */                         \
     pushSTACK(value1);                                                 \
    {var object new_cons = allocate_cons(); /* new cons */              \
     Car(new_cons) = popSTACK(); /* new_cons = (LIST (FUNCALL ...)) */  \
     Cdr(STACK_0) = new_cons; STACK_0 = new_cons; /* lengthens total list */ \
    }}                                                                  \
   fertig:                                                              \
   VALUES1(Cdr(*ergptr)); /* result list without header-cons */         \
   set_args_end_pointer(args_pointer); /* clean up STACK */             \
  }}

#endif

/* macro for MAPC and MAPL */
#define MAPC_MAPL_BODY(listaccess,caller)                               \
  { var object* args_pointer = rest_args_pointer STACKop 2;             \
    argcount++; /* argcount := number of lists on the stack */          \
    /* reserve space for the function call arguments: */                \
    get_space_on_STACK(sizeof(object)*(uintL)argcount);                 \
    pushSTACK(BEFORE(rest_args_pointer)); /* save first list argument */ \
   {var object* ergptr = &STACK_0; /* pointer to it */                  \
    /* traverse all lists in parallel: */                               \
    loop { var object* argptr = args_pointer;                           \
      var object fun = NEXT(argptr);                                    \
      var uintC count;                                                  \
      dotimespC(count,argcount,{                                        \
        var object* next_list_ = &NEXT(argptr);                         \
        var object next_list = *next_list_;                             \
        if (ENDP(caller,next_list)) goto fertig; /* a list ended -> done */ \
        pushSTACK(listaccess(next_list)); /* as argument on the stack */ \
        *next_list_ = Cdr(next_list); /* shorten list */                \
      });                                                               \
      funcall(fun,argcount); /* call function */                        \
    }                                                                   \
    fertig:                                                             \
    VALUES1(*ergptr); /* first list as value */                         \
    set_args_end_pointer(args_pointer); /* clean up STACK */            \
   }}

#ifdef MAP_REVERSES

/* macro for MAPCAN and MAPCON */
#define MAPCAN_MAPCON_BODY(listaccess,caller)                           \
  { var object* args_pointer = rest_args_pointer STACKop 2;             \
    argcount++; /* argcount := number of lists on the stack */          \
    /* reserve space for the function call arguments: */                \
    get_space_on_STACK(sizeof(object)*(uintL)argcount);                 \
    pushSTACK(NIL); /* start of the result list */                      \
   {var object* ergptr = &STACK_0; /* pointer to it */                  \
    /* traverse all lists in parallel: */                               \
    loop { var object* argptr = args_pointer;                           \
      var object fun = NEXT(argptr);                                    \
      var uintC count;                                                  \
      dotimespC(count,argcount,{                                        \
        var object* next_list_ = &NEXT(argptr);                         \
        var object next_list = *next_list_;                             \
        if (ENDP(caller,next_list)) goto fertig; /* a list ended -> done */ \
        pushSTACK(listaccess(next_list)); /* as argument on the stack */ \
        *next_list_ = Cdr(next_list); /* shorten list */                \
      });                                                               \
      funcall(fun,argcount); /* call function */                        \
      STACK_0 = nreconc(value1,STACK_0); /* append result */            \
    }                                                                   \
    fertig:                                                             \
    VALUES1(nreconc(*ergptr,NIL)); /* reverse result list */            \
    set_args_end_pointer(args_pointer); /* clean up STACK */            \
   }}

#else

/* macro for MAPCAN and MAPCON */
#define MAPCAN_MAPCON_BODY(listaccess,caller)                           \
  { var object* args_pointer = rest_args_pointer STACKop 2;             \
    argcount++; /* argcount := number of lists on the stack */          \
    /* reserve space for the function call arguments: */                \
    get_space_on_STACK(sizeof(object)*(uintL)argcount);                 \
    /* start total list: */                                             \
   {var object new_cons = allocate_cons(); /* (CONS NIL NIL) */         \
    pushSTACK(new_cons); /* total list */                               \
    pushSTACK(new_cons); /* (last totallist) */                         \
   }                                                                    \
   {var object* ergptr = &STACK_1; /* pointer to it */                  \
    /* traverse all lists in parallel: */                               \
    loop { var object* argptr = args_pointer;                           \
      var object fun = NEXT(argptr);                                    \
      var uintC count;                                                  \
      dotimespC(count,argcount,{                                        \
        var object* next_list_ = &NEXT(argptr);                         \
        var object next_list = *next_list_;                             \
        if (ENDP(caller,next_list)) goto fertig; /* a list ended -> done */ \
        pushSTACK(listaccess(next_list)); /* as argument on the stack */ \
        *next_list_ = Cdr(next_list); /* shorten list */                \
      });                                                               \
      funcall(fun,argcount); /* call function */                        \
     {var object list = value1; /* list to append */                    \
      if (consp(list)) {                                                \
        Cdr(STACK_0) = list; /* insert as (cdr (last totallist)) */     \
        while (mconsp(Cdr(list))) { list = Cdr(list); }                 \
        STACK_0 = list; /* and (last totallist) := (last list) */       \
      }}}                                                               \
    fertig:                                                             \
    VALUES1(Cdr(*ergptr)); /* result list without header-cons */        \
    set_args_end_pointer(args_pointer); /* clean up STACK */            \
   }}

#endif

#define Identity

LISPFUN(mapcar,2,0,rest,nokey,0,NIL)
/* (MAPCAR fun list {list}), CLTL p. 128 */
  MAPCAR_MAPLIST_BODY(Car,S(mapcar))

LISPFUN(maplist,2,0,rest,nokey,0,NIL)
/* (MAPLIST fun list {list}), CLTL p. 128 */
  MAPCAR_MAPLIST_BODY(Identity,S(maplist))

LISPFUN(mapc,2,0,rest,nokey,0,NIL)
/* (MAPC fun list {list}), CLTL p. 128 */
  MAPC_MAPL_BODY(Car,S(mapc))

LISPFUN(mapl,2,0,rest,nokey,0,NIL)
/* (MAPL fun list {list}), CLTL p. 128 */
  MAPC_MAPL_BODY(Identity,S(mapl))

LISPFUN(mapcan,2,0,rest,nokey,0,NIL)
/* (MAPCAN fun list {list}), CLTL p. 128 */
  MAPCAN_MAPCON_BODY(Car,S(mapcan))

LISPFUN(mapcon,2,0,rest,nokey,0,NIL)
/* (MAPCON fun list {list}), CLTL p. 128 */
  MAPCAN_MAPCON_BODY(Identity,S(mapcon))

LISPSPECFORM(tagbody, 0,0,body)
{ /* (TAGBODY {tag | statement}), CLTL p. 130 */
  var object body = popSTACK();
  { /* build GENV-frame: */
    var object* top_of_frame = STACK; /* pointer to frame */
    pushSTACK(aktenv.go_env);
    finish_frame(ENV1G);
  }
  /* build TAGBODY-frame: */
  var object* top_of_frame = STACK; /* pointer to frame */
  /* parse body and store tags in stack: */
  var uintL tagcount = 0;
  {
    var object body_rest = body;
    while (consp(body_rest)) {
      var object item = Car(body_rest);
      body_rest = Cdr(body_rest);
      /* as tags are considered: symbols and numbers
         (like in compiler), Conses are statements. */
      if (atomp(item)) {
        if (numberp(item) || symbolp(item)) {
          /* store label in stack: */
          check_STACK();
          pushSTACK(body_rest); /* body-rest after the label */
          pushSTACK(item);
          tagcount++;
        } else {
          pushSTACK(item);
          pushSTACK(S(tagbody));
          fehler(source_program_error,
                 GETTEXT("~: ~ is neither tag nor form"));
        }
      }
    }
  }
  if (tagcount>0) {
    var sp_jmp_buf returner; /* return point */
    pushSTACK(aktenv.go_env); /* current GO_ENV as NEXT_ENV */
    finish_entry_frame(ITAGBODY,&!returner,, goto go_entry; );
    /* extend GO_ENV: */
    aktenv.go_env = make_framepointer(STACK);
    if (false) {
     go_entry: /* we jump to this label, if this frame has caught a GO. */
      body = value1; /* the formlist is passed as value1. */
    }
    /* process statements: */
    pushSTACK(body);
    while (mconsp(STACK_0)) {
      var object body_rest = STACK_0;
      STACK_0 = Cdr(body_rest); /* remaining body */
      body_rest = Car(body_rest); /* next item */
      if (consp(body_rest)) {
        eval(body_rest); /* form -> evaluate */
      }
    }
    skipSTACK(1); /* forget body */
    unwind(); /* unwind TAGBODY-frame */
    unwind(); /* unwind GENV-frame */
  } else {
    /* body without -> only PROGN with value NIL */
    skipSTACK(2); /* unwind GENV-frame again, GENV is unchanged */
    pushSTACK(body); implicit_prog();
  }
  VALUES1(NIL);
}

LISPSPECFORM(go, 1,0,nobody)
{ /* (GO tag), CLTL p. 133 */
  var object tag = popSTACK();
  if (!(numberp(tag) || symbolp(tag))) {
    pushSTACK(tag);
    pushSTACK(S(go));
    fehler(source_program_error,GETTEXT("~: illegal tag ~"));
  }
  /* peruse GO_ENV: */
  var object env = aktenv.go_env; /* current GO_ENV */
  var object* FRAME;
  while (framepointerp(env)) {
    /* env is a frame-pointer to a ITAGBODY-frame in the stack. */
    FRAME = uTheFramepointer(env);
    if (framecode(FRAME_(0)) & bit(nested_bit_t)) {
      /* frame already nested */
      env = FRAME_(frame_next_env); break;
    }
    /* search tags in  unnested ITAGBODY-frame: */
    var object* bind_ptr = &FRAME_(frame_bindings); /* pointer below the tag bindings */
    var object* bindend_ptr = STACKpointable(topofframe(FRAME_(0))); /* pointer above the tag bindings */
    do {
      if (eql(*bind_ptr,tag)) { /* tag found? */
        value1 = *(bind_ptr STACKop 1); /* fetch formlist from frame */
        goto found;
      }
      bind_ptr skipSTACKop 2;
    } while (bind_ptr != bindend_ptr);
    env = FRAME_(frame_next_env);
  }
  /* env is an Alist. */
  while (consp(env)) {
    var object tagbody_cons = Car(env);
    var object tagbody_vec = Car(tagbody_cons); /* tag-vector */
    var object* tagptr = &TheSvector(tagbody_vec)->data[0];
    var uintL index = 0;
    var uintL count;
    dotimespL(count,Svector_length(tagbody_vec), {
      if (eql(*tagptr++,tag)) { /* tag found? */
        env = Cdr(tagbody_cons);
        if (eq(env,disabled)) { /* tagbody still active? */
          pushSTACK(tag);
          pushSTACK(S(go));
          fehler(control_error,
                 GETTEXT("~: tagbody for tag ~ has already been left"));
        }
        FRAME = uTheFramepointer(env); /* pointer to the (still active!) frame */
        value1 = FRAME_(frame_bindings+2*index+1); /* formlist */
        goto found;
      }
      index++;
    });
    env = Cdr(env);
  }
  /* env is finished. */
  pushSTACK(tag);
  pushSTACK(S(go));
  fehler(source_program_error,
         GETTEXT("~: no tag named ~ is currently visible"));
  /* tagbody-frame found. FRAME is pointing to it (without typeinfo),
     value1 is the liste of the forms to be executed. */
 found:
  mv_count=1; /* formlist value1 is passed */
  /* jump to the found tagbody-frame and continue there: */
  unwind_upto(FRAME);
}

/* error-message, when there are too many values
 fehler_mv_zuviel(caller);
 > caller: Caller, a symbol */
nonreturning_function(global, fehler_mv_zuviel, (object caller)) {
  pushSTACK(caller);
  fehler(error,GETTEXT("~: too many values"));
}

LISPFUN(values,0,0,rest,nokey,0,NIL)
{ /* (VALUES {arg}), CLTL p. 134 */
  if (argcount >= mv_limit)
    fehler_mv_zuviel(S(values));
  STACK_to_mv(argcount);
}

LISPFUNN(values_list,1)
{ /* (VALUES-LIST list), CLTL p. 135 */
  list_to_mv(popSTACK(), fehler_mv_zuviel(S(values_list)); );
}

LISPSPECFORM(multiple_value_list, 1,0,nobody)
{ /* (MULTIPLE-VALUE-LIST form), CLTL p. 135 */
  eval(popSTACK()); /* evaluate form */
  mv_to_list(); /* pack values into list */
  VALUES1(popSTACK()); /* return list */
}

LISPSPECFORM(multiple_value_call, 1,0,body)
{ /* (MULTIPLE-VALUE-CALL fun {form}), CLTL p. 135 */
  var object* fun_ = &STACK_1;
  *fun_ = (eval(*fun_),value1); /* evaluate function */
  var object forms = popSTACK(); /* formlist */
  var uintL argcount = 0; /* number of arguments so far */
  while (consp(forms)) {
    pushSTACK(Cdr(forms)); /* remaining forms */
    eval(Car(forms)); /* evaluate next form */
    forms = popSTACK();
    /* put its values into the stack: */
    argcount += (uintL)mv_count;
    mv_to_STACK();
  }
  if (((uintL)~(uintL)0 > ca_limit_1) && (argcount > ca_limit_1)) {
    pushSTACK(*fun_);
    pushSTACK(S(multiple_value_call));
    fehler(program_error,GETTEXT("~: too many arguments to ~"));
  }
  funcall(*fun_,argcount); /* call function */
  skipSTACK(1);
}

LISPSPECFORM(multiple_value_prog1, 1,0,body)
{ /* (MULTIPLE-VALUE-PROG1 form {form}), CLTL p. 136 */
  eval(STACK_1); /* evaluate first form */
  var object body = popSTACK();
  skipSTACK(1);
  var uintC mvcount = mv_count; /* number of values */
  mv_to_STACK(); /* all values into the stack */
  pushSTACK(body); implicit_prog();
  STACK_to_mv(mvcount); /* fetch all values again from the stack */
}

LISPSPECFORM(multiple_value_bind, 2,0,body)
{ /* (MULTIPLE-VALUE-BIND ({var}) values-form {decl} {form}), CLTL p. 136 */
  /* separate {decl} {form} : */
  var bool to_compile = parse_dd(STACK_0,aktenv.var_env,aktenv.fun_env); /* incomplete var_env?? */
  /* please no docstring: */
  if (!nullp(value3))
    fehler_docstring(S(multiple_value_bind),STACK_0);
  if (to_compile) { /* declaration (COMPILE) ? */
    /* yes -> compile form: */
    skipSTACK(3); return_Values compile_eval_form();
  } else {
    var object varlist = STACK_2;
    STACK_2 = STACK_1;
    skipSTACK(2);
    /* build variable binding frame, extend VAR_ENV : */
    var object* form_ = &STACK_0;
    var object* bind_ptr;
    var uintC bind_count;
    make_variable_frame(S(multiple_value_bind),varlist,&bind_ptr,&bind_count);
    /* stack layout: values-form, variable binding frame,
                     env-binding-frame, ({form}).
       now evaluate values-form: */
    eval(*form_);
    /* Macro for binding variables in the variable-frame: binds
       the next variable to value, decreases frame_pointer by 2 resp. 3. */
  #define bind_next_var(value)                                          \
    { var object* valptr = &Next(frame_pointer);                        \
      frame_pointer skipSTACKop -varframe_binding_size;                 \
     {var object* markptr = &Before(frame_pointer);                     \
      if (as_oint(*markptr) & wbit(dynam_bit_o)) {                      \
        /* activate dynamic binding: */                                 \
        var object sym = *(markptr STACKop varframe_binding_sym); /* var */ \
        *valptr = TheSymbolflagged(sym)->symvalue; /* old val into the frame */ \
        *markptr = as_object(as_oint(*markptr) | wbit(active_bit_o)); /* activate binding */ \
        TheSymbolflagged(sym)->symvalue = (value); /* new value into the value cell */ \
      } else { /* activate static binding : */                          \
        *valptr = (value); /* new value into the frame */               \
        *markptr = as_object(as_oint(*markptr) | wbit(active_bit_o)); /* activate binding */ \
      }}}
    /* bind the r:=bind_count variables to the s:=mv_count values:
       (if there are not enough variables: discard remaining values;
       if there are not enough values:    fill with NIL.)
       here, r>=0 and s>=0. */
    {
      var object* frame_pointer = bind_ptr;
      var uintC r = bind_count;
      var object* mv_pointer;
      var uintC s = mv_count;
      if (r==0) goto ok; /* no variables? */
      if (s==0) goto fill; /* no values? */
      /* still min(r,s)>0 values to bind: */
     #if !defined(VALUE1_EXTRA)
      mv_pointer = &mv_space[0];
     #else
      bind_next_var(value1);
      if (--r == 0) goto ok; /* no more variables? */
      if (--s == 0) goto fill; /* no more values? */
      mv_pointer = &mv_space[1];
     #endif
      /* still min(r,s)>0 values to bind: */
      loop {
        bind_next_var(*mv_pointer++);
        if (--r == 0) goto ok; /* no more variables? */
        if (--s == 0) goto fill; /* no more values? */
      }
     fill: /* still bind r>0 variables to NIL */
      dotimespC(r,r, { bind_next_var(NIL); });
     ok: ;
    }
    /* interpret body: */
    implicit_progn(popSTACK(),NIL);
    /* unwind frames: */
    unwind(); /* unwind VENV binding frame */
    unwind(); /* unwind variable-binding-frame */
    skipSTACK(1);
  }
}
#undef bind_next_var

LISPSPECFORM(multiple_value_setq, 2,0,nobody)
{ /* (MULTIPLE-VALUE-SETQ ({var}) form), CLTL p. 136 */
  {
    var object varlist = STACK_1;
    /* peruse variable list: */
    while (consp(varlist)) {
      var object symbol =       /* next variable */
        test_symbol_non_constant(S(multiple_value_setq),Car(varlist));
      if (sym_macrop(symbol)) /* and not a symbol-macro */
        goto expand;
      varlist = Cdr(varlist);
    }
  }
  if (false) {
   expand:
    pushSTACK(STACK_0); STACK_1 = STACK_2; STACK_2 = S(multiple_value_setf);
    var object newform = listof(3); /* turn MULTIPLE-VALUE-SETQ into MULTIPLE-VALUE-SETF */
    eval(newform);
  } else {
    eval(popSTACK()); /* evaluate form */
    var object varlist = popSTACK();
    var object* args_end = args_end_pointer;
    mv_to_STACK(); /* write values into the stack (eases access) */
    /* peruse variable-list: */
    var object* mvptr = args_end;
    var uintC count = mv_count; /* number of values that are still available */
    while (consp(varlist)) {
      var object value;
      if (count>0) {
        value = NEXT(mvptr); count--; /* next value */
      } else {
        value = NIL; /* NIL, if all values are consumed */
      }
      setq(Car(varlist),value); /* assign to the next variable */
      varlist = Cdr(varlist);
    }
    set_args_end_pointer(args_end); /* clean up STACK */
    mv_count=1; /* last value1 as only value */
  }
}

LISPSPECFORM(catch, 1,0,body)
{ /* (CATCH tag {form}), CLTL p. 139 */
  STACK_1 = (eval(STACK_1),value1); /* evaluate tag */
  /* finish building of CATCH-frame: */
  var object body = popSTACK(); /* ({form}) */
  var object* top_of_frame = STACK STACKop 1; /* pointer above frame */
  var sp_jmp_buf returner; /* memorize return point */
  finish_entry_frame(CATCH,&!returner,, goto catch_return; );
  /* execute body: */
  implicit_progn(body,NIL);
 catch_return: /* we jump to this label, if the catch-frame built
                  above has caught a throw. */
  skipSTACK(3); /* unwind CATCH-frame */
}

LISPSPECFORM(unwind_protect, 1,0,body)
{ /* (UNWIND-PROTECT form {cleanup}), CLTL p. 140 */
  var object cleanup = popSTACK();
  var object form = popSTACK();
  /* build UNWIND-PROTECT-frame: */
  pushSTACK(cleanup);
  var object* top_of_frame = STACK;
  var sp_jmp_buf returner; /* return point */
  finish_entry_frame(UNWIND_PROTECT,&!returner,, goto throw_save; );
  /* evaluate protected form: */
  eval(form);
  { /* Cleanup after normal termination of the protected form: */
    /* unwind UNWIND-PROTECT-frame: */
    skipSTACK(2);
    cleanup = popSTACK();
    /* save values: */
    var uintC mvcount = mv_count;
    mv_to_STACK();
    /* process cleanup-forms: */
    pushSTACK(cleanup); implicit_prog();
    /* write back values: */
    STACK_to_mv(mvcount);
  }
  return;
 throw_save: /* we jump to this label, if the Unwind-Protect-Frame
                built above has kept a throw.
                save unwind_protect_to_save and jump to it in the end. */
  {
    var restartf_t fun = unwind_protect_to_save.fun;
    var object* arg = unwind_protect_to_save.upto_frame;
    /* Cleanup: */
    /* unwind UNWIND-PROTECT-frame: */
    skipSTACK(2);
    cleanup = popSTACK();
    /* save values: */
    var uintC mvcount = mv_count;
    mv_to_STACK();
    /* process cleanup-forms: */
    pushSTACK(cleanup); implicit_prog();
    /* write back values: */
    STACK_to_mv(mvcount);
    /* and jump further: */
    fun(arg);
  }
}

LISPSPECFORM(throw, 2,0,nobody)
{ /* (THROW tag result), CLTL p. 142 */
  STACK_1 = (eval(STACK_1),value1); /* evaluate tag */
  eval(popSTACK()); /* evaluate result */
  var object tag = popSTACK(); /* evaluated tag */
  throw_to(tag); /* try to throw to this tag */
  /* failed. */
  pushSTACK(tag);
  pushSTACK(S(throw));
  fehler(control_error,GETTEXT("~: there is no CATCHer for tag ~"));
}

LISPFUNN(driver,1)
{ /* (SYS::DRIVER fun) builds a driver-frame, that calls the function
 fun (with 0 arguments) each time. fun is executed in a endless loop
 that can be aborted with GO or THROW . */
  var object* top_of_frame = STACK; /* pointer above frame */
  var sp_jmp_buf returner; /* remember entry point */
  finish_entry_frame(DRIVER,&!returner,,;);
  /* this is the entry point. */
  loop { funcall(STACK_(0+2),0); } /* call fun, endless loop */
}

LISPFUNN(unwind_to_driver,1)
{ /* (SYS::UNWIND-TO-DRIVER top-p)
     UNWIND to the next Driver-Frame or to the top. */
  var object arg = popSTACK();
  if (nullp(arg)) {
    reset();
  } else if (posfixnump(arg)) {
    var uintL count = posfixnum_to_L(arg);
    dotimespL(count, count, { reset(); });
  } else {
    var object* FRAME = STACK;
    var object* driver_frame = STACK;
    while (!eq(FRAME_(0),nullobj)) { /* end of Stack? */
      if (framecode(FRAME_(0)) & bit(frame_bit_t)) {
        if (framecode(FRAME_(0)) == DRIVER_frame_info)
          driver_frame = FRAME;
        FRAME = topofframe(FRAME_(0));
      } else
        FRAME skipSTACKop 1;
    }
    if (eq(FRAME_(1),nullobj)) {
      driver(); quit(); /* STACK completely gone -> restart */
    } else
      unwind_upto(driver_frame);
  }
}

/* Checks an optional macroexpansion-environment in STACK_0.
 > STACK_0: argument
 < STACK_0: macroexpansions-environment #(venv fenv)
 can trigger GC */
local void test_env (void) {
  var object arg = STACK_0;
  if (!boundp(arg)
      || nullp(arg)) { /* required by ANSI CL sections 3.1.1.3.1, 3.1.1.4 */
    STACK_0 = allocate_vector(2); /* vector #(nil nil) as default */
  } else if (!(simple_vector_p(arg) && (Svector_length(arg) == 2))) {
    pushSTACK(arg); /* TYPE-ERROR slot DATUM */
    pushSTACK(O(type_svector2)); /* TYPE-ERROR slot EXPECTED-TYPE */
    pushSTACK(arg);
    fehler(type_error,
           GETTEXT("Argument ~ is not a macroexpansion environment"));
  }
}

LISPFUN(macro_function,1,1,norest,nokey,0,NIL)
{ /* (MACRO-FUNCTION symbol [env]), CLTL p. 144;
     Issue MACRO-FUNCTION-ENVIRONMENT:YES */
  test_env();
  var object env = popSTACK();
  var object symbol = test_symbol(popSTACK());
  var object fundef = sym_function(symbol,TheSvector(env)->data[1]);
  if (fsubrp(fundef)) {
    /* a FSUBR -> search in property list: (GET symbol 'SYS::MACRO) */
    var object got = get(symbol,S(macro)); /* search */
    if (!boundp(got)) /* not found? */
      goto nil;
    value1 = got;
  } else if (macrop(fundef)) { /* #<MACRO expander> ? */
    value1 = TheMacro(fundef)->macro_expander;
  } else { /* SUBR/Closure/FunctionMacro/#<UNBOUND> -> no macrodefinition */
   nil:
    value1 = NIL;
  }
  mv_count=1;
}

LISPFUN(macroexpand,1,1,norest,nokey,0,NIL)
{ /* (MACROEXPAND form [env]), CLTL p. 151 */
  test_env();
  var object env = popSTACK();
  var object form = STACK_0;
  STACK_0 = env; /* save env */
  macroexp0(form,env); /* expand */
  if (!nullp(value2)) { /* something happened? */
    /* yes -> expand to death: */
    do { macroexp0(value1,STACK_0);
    } while (!nullp(value2));
    value2 = T;
  }
  mv_count=2; skipSTACK(1);
}

LISPFUN(macroexpand_1,1,1,norest,nokey,0,NIL)
{ /* (MACROEXPAND-1 form [env]), CLTL p. 151 */
  test_env();
  var object env = popSTACK();
  var object form = popSTACK();
  macroexp0(form,env); /* expand one time */
  mv_count=2;
}

LISPSPECFORM(declare, 0,0,body)
{ /* (DECLARE {decl-spec}), CLTL p. 153 */
  /* ({decl-spec}) already in STACK_0 */
  fehler(source_program_error,
         GETTEXT("declarations ~ are not allowed here"));
}

LISPSPECFORM(the, 2,0,nobody)
{ /* (THE value-type form), CLTL p. 161 */
  eval(STACK_0); /* evaluate form */
  mv_to_list(); /* build value list and save */
  /* stack layout: value-type, form, values.
     call (SYS::%THE values (SYS::TYPE-FOR-DISCRIMINATION value-type))
     for type-check: */
  pushSTACK(STACK_0);
  pushSTACK(STACK_(2+1)); funcall(S(type_for_discrimination),1);
  pushSTACK(value1);
  funcall(S(pthe),2);
  if (nullp(value1)) {
    /* type-check failed */
    pushSTACK(STACK_(2+0)); /* value-type */
    pushSTACK(STACK_(0+1)); /* values */
    pushSTACK(STACK_(1+2)); /* form */
    pushSTACK(S(the));
    fehler(error, /* type_error ?? */
           GETTEXT("~: ~ evaluated to the values ~, not of type ~"));
  }
  /* type-check OK -> return values: */
  list_to_mv(popSTACK(), { fehler_mv_zuviel(S(the)); } );
  skipSTACK(2);
}

LISPFUNN(proclaim,1)
{ /* (PROCLAIM decl-spec) */
  var object declspec = popSTACK();
  if (!consp(declspec)) {
    pushSTACK(declspec);
    pushSTACK(S(proclaim));
    fehler(error,GETTEXT("~: bad declaration ~"));
  }
  var object decltype = Car(declspec); /* declaration type */
  if (eq(decltype,S(special))) { /* SPECIAL */
    while (consp( declspec = Cdr(declspec) )) {
      var object symbol = test_symbol(Car(declspec));
      if (!keywordp(symbol))
        clear_const_flag(TheSymbol(symbol));
      set_special_flag(TheSymbol(symbol));
    }
  } else if (eq(decltype,S(declaration))) { /* DECLARATION */
    while (consp( declspec = Cdr(declspec) )) {
      var object symbol = test_symbol(Car(declspec));
      /* (PUSHNEW symbol (cdr declaration-types)) : */
      if (nullp(memq(symbol,Cdr(O(declaration_types))))) {
        pushSTACK(declspec); pushSTACK(symbol);
        {
          var object new_cons = allocate_cons();
          var object list = O(declaration_types);
          Car(new_cons) = popSTACK(); Cdr(new_cons) = Cdr(list);
          Cdr(list) = new_cons;
          declspec = popSTACK();
        }
      }
    }
  } else if (eq(decltype,S(inline)) || eq(decltype,S(notinline))) {
    /* INLINE, NOTINLINE */
    pushSTACK(decltype);
    while (consp( declspec = Cdr(declspec) )) {
      var object symbol = Car(declspec);
      if (!funnamep(symbol))
        fehler_kein_symbol(S(proclaim),symbol);
      /*(SYS::%PUT (SYS::GET-FUNNAME-SYMBOL symbol) 'SYS::INLINABLE decltype)*/
      pushSTACK(declspec);
      pushSTACK(symbol); funcall(S(get_funname_symbol),1); pushSTACK(value1);
      pushSTACK(S(inlinable));
      pushSTACK(STACK_(1+2));
      funcall(L(put),3);
      declspec = popSTACK();
    }
      skipSTACK(1);
  } else if (eq(decltype,S(constant_inline)) || eq(decltype,S(constant_notinline))) { /* CONSTANT-INLINE, CONSTANT-NOTINLINE */
    pushSTACK(decltype);
    while (consp( declspec = Cdr(declspec) )) {
      var object symbol = Car(declspec);
      if (!symbolp(symbol))
        fehler_kein_symbol(S(proclaim),symbol);
      /* (SYS::%PUT symbol 'SYS::CONSTANT-INLINABLE decltype) : */
      pushSTACK(declspec);
      pushSTACK(symbol); pushSTACK(S(constant_inlinable)); pushSTACK(STACK_(1+2)); funcall(L(put),3);
      declspec = popSTACK();
    }
    skipSTACK(1);
  }
  /* the rest is ignored. */
  VALUES1(NIL);
}

LISPFUNN(eval,1)
{ /* (EVAL form), CLTL p. 321 */
  eval_noenv(popSTACK()); /* evaluate form in empty environment */
}

LISPSPECFORM(load_time_value, 1,1,nobody)
{ /* (LOAD-TIME-VALUE form [read-only-p]), CLTL2 p. 680 */
  var object form = STACK_1;
  skipSTACK(2); /* ignore read-only-p */
  eval_noenv(form); /* evaluate form in empty environment */
  mv_count=1;
}

/* UP: Checks an optional environment-argument for EVALHOOK and APPLYHOOK.
 test_optional_env_arg(&env5);
 > subr_self: Caller (a SUBR)
 < env5: 5 components of the environment
 increases STACK by 1 */
local void test_optional_env_arg (environment_t* env5) {
  var object env = popSTACK(); /* env-argument */
  if (!boundp(env)) { /* not supplied -> void environment */
    env5->var_env   = NIL;
    env5->fun_env   = NIL;
    env5->block_env = NIL;
    env5->go_env    = NIL;
    env5->decl_env  = O(top_decl_env);
  } else if (simple_vector_p(env) && (Svector_length(env) == 5)) {
    /* a simple-vector of length 5 */
    env5->var_env   = TheSvector(env)->data[0];
    env5->fun_env   = TheSvector(env)->data[1];
    env5->block_env = TheSvector(env)->data[2];
    env5->go_env    = TheSvector(env)->data[3];
    env5->decl_env  = TheSvector(env)->data[4];
  } else
    fehler_environment(env);
}

LISPFUN(evalhook,3,1,norest,nokey,0,NIL)
{ /* (EVALHOOK form evalhookfn applyhookfn [env]), CLTL p. 323 */
  var environment_t env5;
  test_optional_env_arg(&env5); /* env-argument after env5 */
  var object applyhookfn = popSTACK();
  var object evalhookfn = popSTACK();
  var object form = popSTACK();
  bindhooks(evalhookfn,applyhookfn); /* bind *EVALHOOK* and *APPLYHOOK* */
  /* build environment-frame: */
  make_ENV5_frame();
  /* set current environments: */
  aktenv = env5;
  /* evaluate form bypassing *EVALHOOK* and *APPLYHOOK* : */
  eval_no_hooks(form);
  unwind(); /* unwind environment-frame */
  unwind(); /* unwind binding frame for *EVALHOOK* / *APPLYHOOK* */
}

LISPFUN(applyhook,4,1,norest,nokey,0,NIL)
{ /* (APPLYHOOK function args evalhookfn applyhookfn [env]), CLTL p. 323 */
  var environment_t env5;
  test_optional_env_arg(&env5); /* env-Argument after env5 */
  var object applyhookfn = popSTACK();
  var object evalhookfn = popSTACK();
  var object args = popSTACK();
  var object fun = popSTACK();
  bindhooks(evalhookfn,applyhookfn); /* bind *EVALHOOK* and *APPLYHOOK* */
  /* build environment-frame: */
  make_ENV5_frame();
  /* set current environments: */
  aktenv = env5;
  { /* save fun: */
    pushSTACK(fun);
    var object* fun_ = &STACK_0;
    /* evaluate each argument and store on the stack: */
    var uintC argcount = 0;
    while (consp(args)) {
      pushSTACK(Cdr(args)); /* remaining argument list */
      eval_no_hooks(Car(args)); /* evaluate next arg */
      args = STACK_0; STACK_0 = value1; /* store value in stack */
      argcount++;
      if (argcount==0) { /* overflow? */
        pushSTACK(*fun_);
        pushSTACK(S(applyhook));
        fehler(program_error,GETTEXT("~: too many arguments given to ~"));
      }
    }
    funcall(*fun_,argcount); /* apply function */
    skipSTACK(1);
  }
  unwind(); /* unwind environment-frame */
  unwind(); /* unwind binding frame for *EVALHOOK* / *APPLYHOOK* */
}

LISPFUN(constantp,1,1,norest,nokey,0,NIL)
{ /* (CONSTANTP expr [env]), CLTL p. 324 */
  skipSTACK(1); /* environment is not used */
  var object arg = popSTACK();
 #ifdef TYPECODES
  switch (typecode(arg))
 #else
  if (orecordp(arg))
    switch (Record_type(arg)) {
      case_Rectype_Symbol_above;
      case_Rectype_number_above;
      case_Rectype_array_above;
      default:
        goto nein;
    }
  else if (consp(arg))
    goto case_cons;
  else if (charp(arg))
    goto case_char;
  else if (immediate_number_p(arg))
    goto case_number;
  else switch (0)
 #endif
    {
      case_cons: /* cons */
      if (eq(Car(arg),S(quote)))
        goto ja;
      else
        goto nein;
      case_symbol: /* symbol */
      if (constantp(TheSymbol(arg)))
        goto ja;
      else
        goto nein;
      case_number: /* number */
      case_char: /* character */
      case_array: /* array */
      goto ja;
      default:
        goto nein;
    }
 ja: VALUES1(T); return;
 nein: VALUES1(NIL); return;
}

LISPFUNN(function_name_p,1)
{ /* (SYS::FUNCTION-NAME-P expr) recognizes function name */
  var object arg = popSTACK();
  VALUES_IF(funnamep(arg));
}

LISPFUN(parse_body,1,2,norest,nokey,0,NIL)
{ /* (SYS::PARSE-BODY body [docstring-allowed [env]])
 parses body, recognizes declarations, returns three values:
 1. body-rest, all forms after the declarations,
 2. list of occurred declspecs
 3. docstring (only if docstring-allowed=T ) or NIL.
 (docstring-allowed should be = NIL or T ,
  env should be a function-environment.) */
  test_env();
  var bool docstring_allowed = !missingp(STACK_1);
  var object body = STACK_2; /* body = ({decl|doc} {form}) */
  STACK_1 = NIL; /* There was no doc-string, yet */
  pushSTACK(NIL); /* Start decl-spec-list */
  /* stack layout: body, docstring, env, declspecs. */
  while (consp(body)) {
    pushSTACK(body); /* save body */
    var object form = Car(body); /* next form */
    /* poss. macroexpand (without expanding FSUBRs, symbols): */
    do {
      var object env = STACK_(1+1);
      macroexp(form,TheSvector(env)->data[0],TheSvector(env)->data[1]);
      form = value1;
    } while (!nullp(value2));
    body = popSTACK();
    var object body_rest = Cdr(body); /* shorten body */
    if (stringp(form)) { /* doc-string found? */
      if (atomp(body_rest)) /* at last position of the form list? */
        goto fertig; /* yes -> last form cannot be a doc-string! */
      if (!docstring_allowed) { /* no doc-string allowed? */
        pushSTACK(STACK_3); /* whole body */
        fehler(source_program_error,
               GETTEXT("no doc-strings allowed here: ~"));
      }
      if (!nullp(STACK_2)) { /* has there already been a doc-string? */
        /* yes -> more than one doc-string is too many: */
        pushSTACK(STACK_3); /* whole body */
        fehler(source_program_error,
               GETTEXT("Too many documentation strings in ~"));
      }
      STACK_2 = form; /* new doc-string */
      body = body_rest;
    } else if (consp(form) && eq(Car(form),S(declare))) { /* (DECLARE ...) ? */
      /* cons new decl-specs to STACK_0 one at a time: */
      pushSTACK(body_rest); /* save body_rest */
      pushSTACK(Cdr(form)); /* list of new decl-specs */
      while (mconsp(STACK_0)) {
        /* cons this declaration to STACK_(0+2) : */
        var object new_cons = allocate_cons();
        Car(new_cons) = Car(STACK_0);
        Cdr(new_cons) = STACK_(0+2);
        STACK_(0+2) = new_cons;
        /* go to next decl-spec: */
        STACK_0 = Cdr(STACK_0);
      }
      skipSTACK(1);
      body = popSTACK(); /* body := old body_rest */
    } else {
     fertig: /* done with looping through the form list */
    #if 0
      /* good idea in the interpreter, but the compiler is hampered,
         because then it cannot compile CASE and HANDLER-BIND so well. */
      if (!eq(form,Car(body))) { /* if the form was expanded, */
        /* replace body with (cons form (cdr body)) : */
        pushSTACK(body_rest); pushSTACK(form);
        body = allocate_cons();
        Car(body) = popSTACK(); /* form */
        Cdr(body) = popSTACK(); /* body_rest */
      }
     #endif
      break;
    }
  }
  /* 3 values: ({form}), declspecs, doc */
  VALUES3(body,
          nreverse(STACK_0), /* decl-spec-list */
          STACK_2); /* doc-string */
  skipSTACK(4);
}

LISPFUNN(keyword_test,2)
{ /* (SYSTEM::KEYWORD-TEST arglist kwlist)
 determines, if all keywords in the list kwlist occur
 in the argument-list arglist (a keyword/value - list) or if there
 is a keyword/value-pair :ALLOW-OTHER-KEYS with value /= NIL .
 if not, error. */
  var object arglist = STACK_1;
  { /* check number of arguments: */
    var uintL argcount = llength(arglist);
    if (!((argcount%2) == 0)) {
      pushSTACK(arglist);
      fehler(program_error,
             GETTEXT("keyword argument list ~ has an odd length"));
    }
  }
  { /* search, if there is :ALLOW-OTHER-KEYS : */
    var object arglistr = arglist;
    while (consp(arglistr)) {
      if (eq(Car(arglistr),S(Kallow_other_keys)) && !nullp(Car(Cdr(arglistr))))
        goto fertig;
      arglistr = Cdr(Cdr(arglistr));
    }
  }
  { /* search, if all specified keywords occur in kwlist: */
    var object arglistr = arglist;
    while (consp(arglistr)) {
      var object key = Car(arglistr);
      if (!nullp(memq(key,STACK_0)))
        goto found;
      /* not found */
      pushSTACK(key); /* KEYWORD-ERROR Slot DATUM */
      pushSTACK(key);
      pushSTACK(STACK_(0+2));
      pushSTACK(Car(Cdr(arglistr)));
      pushSTACK(key);
      {
        var object type = allocate_cons();
        Car(type) = S(member); Cdr(type) = STACK_(0+5);
        STACK_3 = type; /* `(MEMBER ,@kwlist) = KEYWORD-ERROR Slot EXPECTED-TYPE */
      }
      fehler(keyword_error,
             GETTEXT("illegal keyword/value pair ~, ~ in argument list. The allowed keywords are ~"));
     found: /* found. continue: */
      arglistr = Cdr(Cdr(arglistr));
    }
  }
 fertig:
  skipSTACK(2);
  VALUES1(NIL);
}

LISPSPECFORM(and, 0,0,body)
{ /* (AND {form}), CLTL p. 82 */
  var object body = popSTACK();
  if (atomp(body)) {
    VALUES1(T); /* (AND) -> T */
  } else {
    loop {
      pushSTACK(Cdr(body));
      eval(Car(body)); /* evaluate form */
      body = popSTACK();
      if (atomp(body))
        break; /* at the end: return values of the last form */
      if (nullp(value1)) {
        mv_count=1; break; /* prematurely: 1 value NIL */
      }
    }
  }
}

LISPSPECFORM(or, 0,0,body)
{ /* (OR {form}), CLTL p. 83 */
  var object body = popSTACK();
  if (atomp(body)) {
    VALUES1(NIL); /* (OR) -> NIL */
  } else {
    loop {
      pushSTACK(Cdr(body));
      eval(Car(body)); /* evaluate form */
      body = popSTACK();
      if (atomp(body))
        break; /* at the end: return values of the last form */
      if (!nullp(value1)) {
        mv_count=1; break; /* prematurely: 1 value /=NIL */
      }
    }
  }
}

/* From now on, the table macro has a different use: */
#undef LISPSPECFORM

/* table of all Fsubr-functions: */
global const struct fsubr_tab_ fsubr_tab = {
 #define LISPSPECFORM LISPSPECFORM_D
  #include "fsubr.c"
 #undef LISPSPECFORM
};
