# Special-Forms, Kontrollstrukturen, Evaluator-Nahes für CLISP
# Bruno Haible 1990-1999

#include "lispbibl.c"


LISPFUN(exit,0,1,norest,nokey,0,NIL)
# (SYSTEM::%EXIT [errorp]) verlässt das System
  { var object errorp = STACK_0;
    final_exitcode = ((eq(errorp,unbound) || nullp(errorp)) ? 0 : 1);
    quit();
  }

LISPSPECFORM(eval_when, 1,0,body)
# (EVAL-WHEN ({situation}) {form}), CLTL S. 69
  { var object situations = STACK_1; # Liste der Situationen
    # Symbol EVAL oder Liste (NOT COMPILE) darin suchen:
    while (consp(situations))
      { var object situation = Car(situations);
        if (eq(situation,S(eval)) || # Symbol EVAL gefunden?
            eq(situation,S(Kexecute)))
          goto found;
        if (consp(situation) && eq(Car(situation),S(not)))
          { situation = Cdr(situation);
            if (consp(situation) && nullp(Cdr(situation))
                && (eq(Car(situation),S(compile)) || # Liste (NOT COMPILE) gefunden?
                    eq(Car(situation),S(Kcompile_toplevel)))
               )
              goto found;
          }
        situations = Cdr(situations);
      }
    # Symbol EVAL nicht gefunden
    value1 = NIL; mv_count=1; # Wert NIL
    skipSTACK(2);
    return;
    found: # Symbol EVAL gefunden
   {var object body = popSTACK();
    skipSTACK(1);
    implicit_progn(body,NIL); # body auswerten
    return;
  }}

LISPSPECFORM(quote, 1,0,nobody)
# (QUOTE object) == 'object, CLTL S. 86
  { value1 = popSTACK(); mv_count=1; } # Argument als Wert

# Fehlermeldung bei FUNCTION/FLET/LABELS, wenn kein Funktionssymbol vorliegt.
# > caller: Aufrufer, ein Symbol
# > obj: fehlerhaftes Funktionssymbol
  nonreturning_function(local, fehler_funsymbol, (object caller, object obj));
  local void fehler_funsymbol(caller,obj)
    var object caller;
    var object obj;
    { pushSTACK(obj);
      pushSTACK(caller);
      fehler(source_program_error,
             GETTEXT("~: function name ~ should be a symbol")
            );
    }

LISPSPECFORM(function, 1,1,nobody)
# (FUNCTION funname), CLTL. S. 87
# entweder (FUNCTION symbol)
#     oder (FUNCTION (LAMBDA . lambdabody))
#     oder (FUNCTION name (LAMBDA . lambdabody))
  { var object funname; # Funktionsname (Symbol oder Lambdabody)
    var object name; # Name (Symbol)
    if (eq(STACK_0,unbound))
      # 1 Argument
      { funname = STACK_1;
        if (funnamep(funname))
          # (FUNCTION symbol) - Syntax
          { # Symbol im aktuellen Funktions-Environment suchen:
            var object fun = sym_function(funname,aktenv.fun_env);
            # SUBR oder Closure oder Foreign-Function zurückgeben, sonst Fehler:
            if (!(subrp(fun) || closurep(fun) || ffunctionp(fun)))
              { pushSTACK(funname); # Wert für Slot NAME von CELL-ERROR
                pushSTACK(funname);
                pushSTACK(S(function));
                fehler(undefined_function,
                       GETTEXT("~: undefined function ~")
                      );
              }
            value1 = fun; mv_count=1; skipSTACK(2); return;
          }
        name = S(Klambda); # :LAMBDA als Default-Name
      }
      else
      # 2 Argumente
      { name = STACK_1; # 1. Argument
        if (!funnamep(name)) { fehler_funsymbol(S(function),name); }
        funname = STACK_0; # 2. Argument, hoffentlich Lambdaausdruck
      }
    if (!(consp(funname) && eq(Car(funname),S(lambda)))) # Cons (LAMBDA . ...) ?
      { pushSTACK(funname);
        pushSTACK(S(function));
        fehler(source_program_error,
               GETTEXT("~: ~ is not a function name")
              );
      }
    # Lambdaausdruck
    # im aktuellen Environment in eine Closure umwandeln:
    value1 = get_closure(Cdr(funname),name,FALSE,&aktenv); mv_count=1;
    skipSTACK(2); return;
  }

# Fehler, wenn ein Symbol keinen Wert hat.
# > symbol: Symbol
# > subr_self: Aufrufer (ein SUBR)
  nonreturning_function(local, fehler_no_value, (object symbol));
  local void fehler_no_value(symbol)
    var object symbol;
    { pushSTACK(symbol); # Wert für Slot NAME von CELL-ERROR
      pushSTACK(symbol);
      pushSTACK(TheSubr(subr_self)->name);
      fehler(unbound_variable,
             GETTEXT("~: ~ has no dynamic value")
            );
    }

LISPFUNN(psymbol_value,1)
# (SYS::%SYMBOL-VALUE symbol), CLTL S. 90
  { var object symbol = popSTACK();
    if (!symbolp(symbol)) { fehler_symbol(symbol); }
   {var object val = Symbol_value(symbol);
    if (eq(val,unbound)) { fehler_no_value(symbol); }
    value1 = val; mv_count=1;
  }}

LISPFUNN(symbol_value,1)
# (SYMBOL-VALUE symbol), CLTL S. 90
  { var object symbol = popSTACK();
    if (!symbolp(symbol)) { fehler_symbol(symbol); }
   {var object val = Symbol_value(symbol);
    if (eq(val,unbound)) { fehler_no_value(symbol); }
    if (symbolmacrop(val)) # Symbol-Macro?
      # ja -> expandieren und evaluieren:
      { eval_noenv(TheSymbolmacro(val)->symbolmacro_expansion); mv_count=1; }
      else
      { value1 = val; mv_count=1; }
  }}

# Fehlermeldung wegen undefinierter Funktion.
# fehler_undef_function(caller,symbol);
# > caller: Aufrufer (ein Symbol)
# > symbol: Symbol oder (SETF symbol)
  nonreturning_function(global, fehler_undef_function, (object caller, object symbol));
  global void fehler_undef_function(caller,symbol)
    var object caller;
    var object symbol;
    { pushSTACK(symbol); # Wert für Slot NAME von CELL-ERROR
      pushSTACK(symbol);
      pushSTACK(caller);
      fehler(undefined_function,
             GETTEXT("~: ~ has no global function definition")
            );
    }

LISPFUNN(symbol_function,1)
# (SYMBOL-FUNCTION symbol), CLTL S. 90
  { var object symbol = popSTACK();
    if (!symbolp(symbol)) { fehler_symbol(symbol); }
   {var object val = Symbol_function(symbol);
    if (eq(val,unbound)) { fehler_undef_function(S(symbol_value),symbol); }
    value1 = val; mv_count=1;
  }}

LISPFUNN(fdefinition,1)
# (FDEFINITION funname), CLTL2 S. 120
  { var object funname = popSTACK();
    var object symbol = funname;
    if (!funnamep(symbol)) { fehler_symbol(symbol); }
    if (!symbolp(symbol))
      { symbol = get(Car(Cdr(symbol)),S(setf_function)); # (get ... 'SYS::SETF-FUNCTION)
        if (!symbolp(symbol)) # sollte (uninterniertes) Symbol sein
          { fehler_undef_function(S(fdefinition),funname); } # sonst undefiniert
      }
   {var object val = Symbol_function(symbol);
    if (eq(val,unbound))
      { fehler_undef_function(S(fdefinition),funname); }
    value1 = val; mv_count=1;
  }}

LISPFUNN(boundp,1)
# (BOUNDP symbol), CLTL S. 90
  { var object symbol = popSTACK();
    if (!symbolp(symbol)) { fehler_symbol(symbol); }
    value1 = (eq(Symbol_value(symbol),unbound) ? NIL : T); mv_count=1;
  }

LISPFUNN(fboundp,1)
# (FBOUNDP symbol), CLTL S. 90, CLTL2 S. 120
  { var object symbol = popSTACK();
    if (!funnamep(symbol)) { fehler_symbol(symbol); }
    if (!symbolp(symbol))
      { symbol = get(Car(Cdr(symbol)),S(setf_function)); # (get ... 'SYS::SETF-FUNCTION)
        if (!symbolp(symbol)) # sollte (uninterniertes) Symbol sein
          goto undef; # sonst undefiniert
      }
    if (eq(Symbol_function(symbol),unbound))
      { undef: value1 = NIL; }
      else
      { value1 = T; }
    mv_count=1;
  }

LISPFUNN(special_operator_p,1)
# (SPECIAL-OPERATOR-P symbol), was (SPECIAL-FORM-P symbol), CLTL S. 91
  { var object symbol = popSTACK();
    if (!symbolp(symbol)) { fehler_symbol(symbol); }
   {var object obj = Symbol_function(symbol);
    value1 = (fsubrp(obj) ? T : NIL); mv_count=1;
  }}

# Fehlermeldung bei Zuweisung, wenn ein Symbol eine Konstante ist.
# (Einer Konstante kann nicht zugewiesen werden.)
# fehler_symbol_constant(caller,symbol);
# > caller: Aufrufer (ein Symbol)
# > symbol: konstantes Symbol
  nonreturning_function(local, fehler_symbol_constant, (object caller, object symbol));
  local void fehler_symbol_constant(caller,symbol)
    var object caller;
    var object symbol;
    { pushSTACK(symbol);
      pushSTACK(caller);
      fehler(error,
             GETTEXT("~: the value of the constant ~ may not be altered")
            );
    }

# UP: überprüft den Body einer SETQ- oder PSETQ-Form.
# > caller: Aufrufer (ein Symbol)
# > STACK_0: Body
# < ergebnis: TRUE falls Symbol-Macros zu expandieren sind.
  local boolean check_setq_body (object caller);
  local boolean check_setq_body(caller)
    var object caller;
    { var object body = STACK_0;
      while (consp(body))
        { var object symbol = Car(body); # Variable
          if (!symbolp(symbol)) { fehler_kein_symbol(caller,symbol); }
          if (constantp(TheSymbol(symbol)))
            { fehler_symbol_constant(caller,symbol); }
          if (sym_macrop(symbol))
            { return TRUE; }
          body = Cdr(body);
          if (atomp(body))
            { if (!nullp(body)) goto fehler_dotted;
              # Der ganze Body noch in STACK_0.
              pushSTACK(caller);
              fehler(source_program_error,
                     GETTEXT("~ called with odd number of arguments: ~")
                    );
            }
          body = Cdr(body);
        }
      # body ist zu Ende.
      if (!nullp(body))
        { fehler_dotted: # Der ganze Body noch in STACK_0.
          pushSTACK(caller);
          fehler(source_program_error,
                 GETTEXT("dotted list given to ~ : ~")
                );
        }
      return FALSE;
    }

LISPSPECFORM(setq, 0,0,body)
# (SETQ {var form}), CLTL S. 91
  { if (check_setq_body(S(setq)))
      { var object form = allocate_cons();
        Car(form) = S(setf); Cdr(form) = popSTACK(); # aus SETQ mache SETF
        eval(form);
      }
      else
      { var object body = popSTACK();
        if (consp(body))
          { do { var object symbol = Car(body); # Variable
                 body = Cdr(body);
                 pushSTACK(Cdr(body)); # Restliste retten
                 pushSTACK(symbol); # Symbol retten
                 eval(Car(body)); # nächste Form auswerten
                 symbol = popSTACK();
                 setq(symbol,value1); # Zuweisung durchführen
                 body = popSTACK();
               }
               while (consp(body));
            # value1 ist noch das letzte Auswertungs-Ergebnis.
          }
          else
          { value1 = NIL; } # Defaultwert bei (SETQ)
        mv_count=1;
  }   }

LISPSPECFORM(psetq, 0,0,body)
# (PSETQ {var form}), CLTL S. 92
  { if (check_setq_body(S(psetq)))
      { var object form = allocate_cons();
        Car(form) = S(psetf); Cdr(form) = popSTACK(); # aus PSETQ mache PSETF
        eval(form);
      }
      else
      { var object body = popSTACK();
        var uintL body_length = llength(body)/2; # Anzahl der Paare (var form)
        if (body_length > 0)
          { get_space_on_STACK(body_length*2*sizeof(object)); # Platz im STACK belegen
            { var uintL count;
              dotimespL(count,body_length,
                { pushSTACK(Car(body)); # Variable auf den Stack
                  body = Cdr(body);
                  pushSTACK(Cdr(body)); # Restliche Liste auf den Stack
                  eval(Car(body)); # nächste Form auswerten
                  body = STACK_0;
                  STACK_0 = value1; # ihr Ergebnis in den Stack
                });
            }
            { var uintL count;
              dotimespL(count,body_length,
                { var object val = popSTACK(); # Wert
                  var object sym = popSTACK(); # Symbol
                  setq(sym,val); # Zuweisung durchführen
                });
          } }
        value1 = NIL; mv_count=1; # Wert NIL
  }   }

LISPFUNN(set,2)
# (SETF (SYMBOL-VALUE symbol) value) = (SET symbol value), CLTL S. 92
  { var object symbol = STACK_1;
    if (!symbolp(symbol)) { fehler_symbol(symbol); }
    if (constantp(TheSymbol(symbol))) # Konstante?
      { fehler_symbol_constant(S(set),symbol); }
    if (symbolmacrop(Symbol_value(symbol))) # Symbol-Macro?
      # Evaluiere `(SETF ,expansion (QUOTE ,value))
      { pushSTACK(S(setf));
        pushSTACK(TheSymbolmacro(Symbol_value(symbol))->symbolmacro_expansion);
        pushSTACK(S(quote)); pushSTACK(STACK_(0+3)); pushSTACK(listof(2));
        eval_noenv(listof(3)); mv_count=1;
      }
      else
      { value1 = Symbol_value(symbol) = STACK_0; mv_count=1; }
    skipSTACK(2);
  }

LISPFUNN(makunbound,1)
# (MAKUNBOUND symbol), CLTL S. 92
  { var object symbol = popSTACK();
    if (!symbolp(symbol)) { fehler_symbol(symbol); }
    if (constantp(TheSymbol(symbol)))
      { pushSTACK(symbol);
        pushSTACK(S(makunbound));
        fehler(error,
               GETTEXT("~: the value of the constant ~ must not be removed")
              );
      }
    Symbol_value(symbol) = unbound;
    value1 = symbol; mv_count=1;
  }

LISPFUNN(fmakunbound,1)
# (FMAKUNBOUND symbol), CLTL S. 92, CLTL2 S. 123
  { var object funname = popSTACK();
    var object symbol = funname;
    if (!funnamep(symbol)) { fehler_symbol(symbol); }
    if (!symbolp(symbol))
      { symbol = get(Car(Cdr(symbol)),S(setf_function)); # (get ... 'SYS::SETF-FUNCTION)
        if (!symbolp(symbol)) # sollte (uninterniertes) Symbol sein
          goto undef; # sonst undefiniert
      }
    { var object obj = Symbol_function(symbol);
      if (fsubrp(obj))
        { pushSTACK(symbol);
          pushSTACK(S(fmakunbound));
          fehler(error,
                 GETTEXT("~: the special form definition of ~ must not be removed")
                );
    }   }
    Symbol_function(symbol) = unbound;
    undef: value1 = funname; mv_count=1;
  }

LISPFUN(apply,2,0,rest,nokey,0,NIL)
# (APPLY function {arg} arglist), CLTL S. 107
  { BEFORE(rest_args_pointer);
    apply(Before(rest_args_pointer), # function
          argcount, # Anzahl der {arg} auf dem Stack
          popSTACK() # arglist
         );
    skipSTACK(1); # function aus dem Stack entfernen
  }

LISPFUN(pfuncall,1,0,rest,nokey,0,NIL)
# (SYS::%FUNCALL function {arg})
  { funcall(Before(rest_args_pointer),argcount); skipSTACK(1); }

LISPFUN(funcall,1,0,rest,nokey,0,NIL)
# (FUNCALL function {arg}), CLTL S. 108
  { funcall(Before(rest_args_pointer),argcount); skipSTACK(1); }

LISPSPECFORM(progn, 0,0,body)
# (PROGN {form}), CLTL S. 109
  { implicit_progn(popSTACK(),NIL); }

# Macro: Wertet die Formen einer Formenliste aus.
# implicit_prog();
# > -(STACK): Formenliste
# erhöht STACK um 1
# can trigger GC
  #define implicit_prog()  \
    { while (mconsp(STACK_0))                         \
        { var object forms = STACK_0;                 \
          STACK_0 = Cdr(forms);                       \
          eval(Car(forms)); # nächste Form evaluieren \
        }                                             \
      skipSTACK(1);                                   \
    }

LISPSPECFORM(prog1, 1,0,body)
# (PROG1 form1 {form}), CLTL S. 109
  { STACK_1 = (eval(STACK_1),value1); # form1 evaluieren, Wert retten
    implicit_prog();
    value1 = popSTACK(); mv_count=1; # geretteten Wert zurückgeben
  }

LISPSPECFORM(prog2, 2,0,body)
# (PROG2 form1 form2 {form}), CLTL S. 109
  { eval(STACK_2); # form1 evaluieren
    eval(STACK_1); STACK_2 = value1; # form2 evaluieren, Wert retten
    STACK_1 = STACK_0; skipSTACK(1);
    implicit_prog();
    value1 = popSTACK(); mv_count=1; # geretteten Wert zurückgeben
  }

# Fehlermeldung wegen nicht erlaubter Docstrings
# fehler_docstring(caller,body);
# > caller: Aufrufer, ein Symbol
# > body: gesamter Body
  nonreturning_function(local, fehler_docstring, (object caller, object body));
  local void fehler_docstring(caller,body)
    var object caller;
    var object body;
    { pushSTACK(body);
      pushSTACK(caller);
      fehler(source_program_error,
             GETTEXT("~: doc-strings are not allowed here: ~")
            );
    }

# UP für LET, LET*, LOCALLY, MULTIPLE-VALUE-BIND, SYMBOL-MACROLET:
# Kompiliert die aktuelle Form und führt sie in kompiliertem Zustand aus.
# compile_form()
# > im STACK: EVAL-Frame mit der Form
# < mv_count/mv_space: Werte
# can trigger GC
  local Values compile_eval_form (void);
  local Values compile_eval_form()
    { # (SYS::COMPILE-FORM form venv fenv benv genv denv) ausführen:
      # Die ganze Form aus dem EVAL-Frame im Stack holen:
      pushSTACK(STACK_(frame_form)); # als 1. Argument
     {var environment* stack_env = nest_aktenv(); # aktuelles Environment nesten, auf den STACK legen
      #if !defined(STACK_UP)
      var environment my_env;
      my_env = *stack_env; # und hierher übertragen
      skipSTACK(5); # und wieder vom STACK nehmen
      pushSTACK(my_env.var_env); # 2. Argument
      pushSTACK(my_env.fun_env); # 3. Argument
      pushSTACK(my_env.block_env); # 4. Argument
      pushSTACK(my_env.go_env); # 5. Argument
      pushSTACK(my_env.decl_env); # 6. Argument
      #endif
      funcall(S(compile_form),6);
     }# Die sich ergebende compilierte Closure mit 0 Argumenten aufrufen:
      funcall(value1,0);
    }

# UP für LET, LET*, LOCALLY, MULTIPLE-VALUE-BIND, SYMBOL-MACROLET:
# Analysiert die Variablen und Deklarationen, baut einen Variablenbindungs-
# Frame auf und erweitert VENV und evtl. auch DENV durch einen Frame.
# make_variable_frame(caller,varspecs,&bind_ptr,&bind_count)
# > object caller: Aufrufer, ein Symbol
# > object varspecs: Liste von Variablen-Specifiern
# > object value2: Liste von Declaration-Specifiern
# > object value1: Liste ({form}) von Formen
# < Stackaufbau: Variablenbindungsframe, Env-Bindungs-Frame, ({form}).
# < object* bind_ptr: Pointer über die erste "richtige" Bindung.
# < uintC bind_count: Anzahl der "richtigen" Bindungen.
# verändert STACK
# can trigger GC
  local void make_variable_frame (object caller, object varspecs, object** bind_ptr_, uintC* bind_count_);
  local void make_variable_frame(caller,varspecs,bind_ptr_,bind_count_)
    var object caller;
    var object varspecs;
    var object** bind_ptr_;
    var uintC* bind_count_;
    { var object declarations = value2;
      # Variablenbindungs-Frame aufbauen:
      { var object* top_of_frame = STACK; # Pointer übern Frame
        # zuerst die Special-deklarierten Variablen aus declarations
        # im Stack ablegen:
        var object* spec_pointer = args_end_pointer;
        var uintL spec_anz = 0; # Anzahl der SPECIAL-Referenzen
        { var object declspecs = declarations;
          while (consp(declspecs))
            { var object declspec = Car(declspecs); # nächste Deklaration
              if (consp(declspec) && eq(Car(declspec),S(special))) # (SPECIAL ...) ?
                { while (consp( declspec = Cdr(declspec) ))
                    { var object declsym = Car(declspec); # nächstes Special-deklariertes Item
                      if (!symbolp(declsym)) # sollte ein Symbol sein
                        { pushSTACK(declsym);
                          pushSTACK(caller);
                          fehler(source_program_error,
                                 GETTEXT("~: ~ is not a symbol, but was declared SPECIAL")
                                );
                        }
                      # Special-deklariertes Symbol im Stack ablegen:
                      pushSTACK(specdecl); # SPECDECL als "Wert"
                      pushSTACK_symbolwithflags(declsym,wbit(active_bit_o)); # Symbol aktiv
                      check_STACK();
                      spec_anz++;
                }   }
              declspecs = Cdr(declspecs);
        }   }
        *bind_ptr_ = args_end_pointer; # Pointer über erste "richtige" Bindung
        # Dann die "richtigen" Variablenbindungen (jeweils die Variable
        # und ihren unausgewerteten Init) im Stack ablegen:
       {var uintL var_anz = 0; # Anzahl der Variablenbindungen
        { while (consp(varspecs))
            { var object varspec = Car(varspecs); # nächstes varspec
              # in Symbol und Init aufspalten:
              var object symbol;
              var object init;
              if (symbolp(varspec) && !eq(caller,S(symbol_macrolet))) # Symbol ?
                { symbol = varspec; init = unbound; }
              elif # ein-/zweielementige Liste, mit Symbol als CAR ?
                   (consp(varspec)
                    && !eq(caller, S(multiple_value_bind))
                    && (symbol = Car(varspec), varspec = Cdr(varspec),
                        symbolp(symbol) &&
                        ( # zweielementig?
                          (consp(varspec) && nullp(Cdr(varspec))
                           && (init = Car(varspec), TRUE))
                          || # einelementig (bei LET, LET* gemäß X3J13 vote <182> erlaubt)
                          (nullp(varspec) && !eq(caller,S(symbol_macrolet))
                           && (init = NIL, TRUE))
                   )   ))
                {} # now init = Car(varspec) or = NIL
              else
                { pushSTACK(Car(varspecs));
                  pushSTACK(caller);
                  fehler(source_program_error,
                         GETTEXT("~: illegal variable specification ~")
                        );
                }
              pushSTACK(init); # Init und
              pushSTACK_symbolwithflags(symbol,0); # Variable ablegen
              check_STACK();
              # feststellen, ob statische oder dynamische Bindung:
             {var boolean specdecled = FALSE; # Variable unter den Special-deklarierten?
              if (spec_anz > 0)
                {
                  #ifdef NO_symbolflags
                  var object* ptr = spec_pointer;
                  var uintL count;
                  dotimespL(count,spec_anz,
                    { NEXT(ptr);
                      if (eq(NEXT(ptr),symbol))
                        { if (eq(NEXT(ptr),fixnum(bit(active_bit)))) { specdecled = TRUE; break; } }
                        else
                        { NEXT(ptr); }
                    });
                  #else
                  var object to_compare = as_object(as_oint(symbol) | wbit(active_bit_o));
                  var object* ptr = spec_pointer;
                  var uintL count;
                  dotimespL(count,spec_anz,
                    { NEXT(ptr);
                      if (eq(NEXT(ptr),to_compare)) { specdecled = TRUE; break; }
                    });
                  #endif
                }
              if (eq(caller,S(symbol_macrolet)))
                { if (constantp(TheSymbol(symbol)) || special_var_p(TheSymbol(symbol)))
                    { pushSTACK(symbol);
                      pushSTACK(caller);
                      fehler(program_error,
                             GETTEXT("~: symbol ~ is declared special and must not be declared a macro")
                            );
                    }
                  if (specdecled)
                    { pushSTACK(symbol);
                      pushSTACK(caller);
                      fehler(source_program_error,
                             GETTEXT("~: symbol ~ must not be declared SPECIAL and a macro at the same time")
                            );
                    }
                  # statische Bindung
                }
                else
                { if (constantp(TheSymbol(symbol)))
                    { pushSTACK(symbol);
                      pushSTACK(caller);
                      fehler(program_error,
                             GETTEXT("~: ~ is a constant, cannot be bound")
                            );
                    }
                  if (specdecled || special_var_p(TheSymbol(symbol)))
                    # dynamisch binden
                    { STACK_0 = as_object(as_oint(STACK_0) | wbit(dynam_bit_o)); }
                    else
                    # statisch binden
                    {}
                }
              varspecs = Cdr(varspecs);
              var_anz++;
        }   }}
        *bind_count_ = var_anz;
        var_anz += spec_anz; # Gesamtzahl Symbol/Wert-Paare
        #ifndef UNIX_DEC_ULTRIX_GCCBUG
        if (var_anz > (uintC)(~(uintC)0)) # passt es in ein uintC ?
          { pushSTACK(caller);
            fehler(source_program_error,
                   GETTEXT("~: too many variables and/or declarations")
                  );
          }
        #endif
        pushSTACK(aktenv.var_env); # aktuelles VAR_ENV als NEXT_ENV
        pushSTACK(as_object(var_anz)); # Anzahl Bindungen
        finish_frame(VAR);
      }}
      # Der Variablenbindungsframe ist jetzt fertig.
     {var object* var_frame_ptr = STACK; # Pointer auf Variablenbindungs-Frame
      # VENV-Bindungsframe aufbauen:
      { var object* top_of_frame = STACK; # Pointer übern Frame
        # Zuerst DENV um die nötigen declspecs erweitern:
        var object denv = aktenv.decl_env;
        pushSTACK(value1); # ({form}) retten
        pushSTACK(declarations);
        while (mconsp(STACK_0))
          { var object declspecs = STACK_0;
            STACK_0 = Cdr(declspecs);
           {var object declspec = Car(declspecs); # nächstes Declspec
            if (consp(declspec)) # sollte ein Cons sein
              { if (!eq(Car(declspec),S(special))) # (SPECIAL ...) haben wir schon behandelt
                  { denv = augment_decl_env(declspec,denv); } # alles andere behandeln
          }}  }
        skipSTACK(1);
       {var object forms = popSTACK();
        # Nun den Frame bauen:
        if (eq(denv,aktenv.decl_env))
          { pushSTACK(aktenv.var_env);
            finish_frame(ENV1V);
          }
          else
          { pushSTACK(aktenv.decl_env);
            pushSTACK(aktenv.var_env);
            finish_frame(ENV2VD);
            aktenv.decl_env = denv;
          }
        # VENV-Bindungsframe ist fertig.
        aktenv.var_env = make_framepointer(var_frame_ptr); # Pointer auf Variablenbindungsframe
        pushSTACK(forms);
    }}}}

LISPSPECFORM(let, 1,0,body)
# (LET ({varspec}) {decl} {form}), CLTL S. 110
  { # {decl} {form} trennen:
    var boolean to_compile = parse_dd(STACK_0,aktenv.var_env,aktenv.fun_env); # unvollständiges var_env??
    # bitte kein Docstring:
    if (!nullp(value3)) { fehler_docstring(S(let),STACK_0); }
    if (to_compile) # Deklaration (COMPILE) ?
      # ja -> Form kompilieren:
      { skipSTACK(2); return_Values compile_eval_form(); }
      else
      { skipSTACK(1);
        # Variablenbindungsframe aufbauen, VAR_ENV erweitern:
       {var object* bind_ptr;
        var uintC bind_count;
        make_variable_frame(S(let),popSTACK(),&bind_ptr,&bind_count);
        if (bind_count > 0)
          { # Dann die Initialisierungsformen auswerten:
            { var object* frame_pointer = bind_ptr;
              var uintC count;
              dotimespC(count,bind_count,
                { var object* initptr = &NEXT(frame_pointer);
                  var object init = *initptr; # nächstes Init
                  *initptr = (eq(init,unbound) ? NIL : (eval(init),value1)); # auswerten, NIL als Default
                  frame_pointer skipSTACKop -(varframe_binding_size-1);
                });
            }
            # Dann die Bindungen aktivieren:
            { var object* frame_pointer = bind_ptr;
              var uintC count;
              dotimespC(count,bind_count,
                { frame_pointer skipSTACKop -varframe_binding_size;
                 {var object* markptr = &Before(frame_pointer);
                  if (as_oint(*markptr) & wbit(dynam_bit_o)) # Bindung dynamisch?
                    { var object symbol = *(markptr STACKop varframe_binding_sym); # Variable
                      var object newval = *(markptr STACKop varframe_binding_value); # neuer Wert
                      *(markptr STACKop varframe_binding_value) = TheSymbolflagged(symbol)->symvalue; # alten Wert im Frame sichern
                      *markptr = as_object(as_oint(*markptr) | wbit(active_bit_o)); # Bindung aktivieren
                      TheSymbolflagged(symbol)->symvalue = newval; # neuer Wert
                    }
                    else
                    { *markptr = as_object(as_oint(*markptr) | wbit(active_bit_o)); } # Bindung aktivieren
                }});
          } }
        # Body abinterpretieren:
        implicit_progn(popSTACK(),NIL);
        # Frames auflösen:
        unwind(); # VENV-Bindungsframe auflösen
        unwind(); # Variablenbindungs-Frame auflösen
  }   }}

LISPSPECFORM(letstern, 1,0,body)
# (LET* ({varspec}) {decl} {form}), CLTL S. 111
  { # {decl} {form} trennen:
    var boolean to_compile = parse_dd(STACK_0,aktenv.var_env,aktenv.fun_env); # unvollständiges var_env??
    # bitte kein Docstring:
    if (!nullp(value3)) { fehler_docstring(S(letstern),STACK_0); }
    if (to_compile) # Deklaration (COMPILE) ?
      # ja -> Form kompilieren:
      { skipSTACK(2); return_Values compile_eval_form(); }
      else
      { skipSTACK(1);
        # Variablenbindungsframe aufbauen, VAR_ENV erweitern:
       {var object* bind_ptr;
        var uintC bind_count;
        make_variable_frame(S(letstern),popSTACK(),&bind_ptr,&bind_count);
        # Dann die Initialisierungsformen auswerten und die Bindungen aktivieren:
        if (bind_count > 0)
          { var object* frame_pointer = bind_ptr;
            var uintC count;
            dotimespC(count,bind_count,
              { var object* initptr = &Next(frame_pointer);
                frame_pointer skipSTACKop -varframe_binding_size;
               {var object* markptr = &Before(frame_pointer);
                var object init = *initptr; # nächstes Init
                var object newval = (eq(init,unbound) ? NIL : (eval(init),value1)); # auswerten, NIL als Default
                if (as_oint(*markptr) & wbit(dynam_bit_o)) # Bindung dynamisch?
                  { var object symbol = *(markptr STACKop varframe_binding_sym); # Variable
                    *initptr = TheSymbolflagged(symbol)->symvalue; # alten Wert im Frame sichern
                    *markptr = as_object(as_oint(*markptr) | wbit(active_bit_o)); # Bindung aktivieren
                    TheSymbolflagged(symbol)->symvalue = newval; # neuer Wert
                  }
                  else
                  { *initptr = newval; # neuen Wert in den Frame
                    *markptr = as_object(as_oint(*markptr) | wbit(active_bit_o)); # Bindung aktivieren
                  }
              }});
          }
        # Body abinterpretieren:
        implicit_progn(popSTACK(),NIL);
        # Frames auflösen:
        unwind(); # VENV-Bindungsframe auflösen
        unwind(); # Variablenbindungs-Frame auflösen
  }   }}

LISPSPECFORM(locally, 0,0,body)
# (LOCALLY {decl} {form}), CLTL2 S. 221
  { # {decl} {form} trennen:
    var boolean to_compile = parse_dd(STACK_0,aktenv.var_env,aktenv.fun_env); # unvollständiges var_env??
    # bitte kein Docstring:
    if (!nullp(value3)) { fehler_docstring(S(locally),STACK_0); }
    skipSTACK(1);
    if (to_compile) # Deklaration (COMPILE) ?
      # ja -> Form kompilieren:
      { return_Values compile_eval_form(); }
      else
      { # Variablenbindungsframe aufbauen, VAR_ENV erweitern:
        var object* bind_ptr;
        var uintC bind_count;
        make_variable_frame(S(locally),NIL,&bind_ptr,&bind_count);
        # Body abinterpretieren:
        implicit_progn(popSTACK(),NIL);
        # Frames auflösen:
        unwind(); # VENV-Bindungsframe auflösen
        unwind(); # Variablenbindungs-Frame auflösen
  }   }

LISPSPECFORM(compiler_let, 1,0,body)
# (COMPILER-LET ({varspec}) {form}), CLTL S. 112
  { var object* varspecs_ = &STACK_1;
    var object varspecs = *varspecs_; # Liste der Variablen
    var uintL varcount = llength(varspecs); # Anzahl der Variablen
    get_space_on_STACK(varcount*3*sizeof(object)); # Platz auf dem STACK verlangen
    # varspecs evaluieren:
   {var object* val_pointer = args_end_pointer; # Pointer über die Werte
    while (consp(varspecs))
      { var object varspec = Car(varspecs);
        var object symbol;
        if (consp(varspec))
          # varspec ist ein Cons
          { symbol = Car(varspec);
            varspec = Cdr(varspec);
            if (consp(varspec) && nullp(Cdr(varspec)))
              { varspec = Car(varspec); } # Initform = zweites Listenelement
            elif (nullp(varspec)) # allowed by X3J13 vote <182>
              { /* varspec = NIL; */ } # Initform = NIL
            else
              { pushSTACK(Car(varspecs));
                pushSTACK(S(compiler_let));
                fehler(source_program_error,
                       GETTEXT("~: illegal variable specification ~")
                      );
              }
            # symbol sollte ein nichtkonstantes Symbol sein:
            if (!symbolp(symbol))
              { fehler_symbol:
                fehler_kein_symbol(S(compiler_let),symbol);
              }
            if (constantp(TheSymbol(symbol)))
              { fehler_constant:
                pushSTACK(symbol);
                pushSTACK(S(compiler_let));
                fehler(program_error,
                       GETTEXT("~: ~ is a constant, cannot be bound")
                      );
              }
            pushSTACK(Cdr(varspecs));
            eval_noenv(varspec); # Initform auswerten
            varspecs = STACK_0;
            STACK_0 = value1; # und in den Stack
          }
          else
          { symbol = varspec;
            if (!symbolp(symbol)) goto fehler_symbol;
            if (constantp(TheSymbol(symbol))) goto fehler_constant;
            pushSTACK(NIL); # NIL als Wert in den Stack
            varspecs = Cdr(varspecs);
      }   }
    varspecs = *varspecs_;
    # Frame aufbauen:
    { var object* top_of_frame = STACK; # Pointer übern Frame
      while (consp(varspecs))
        { var object varspec = Car(varspecs);
          if (consp(varspec)) { varspec = Car(varspec); }
          pushSTACK(Symbol_value(varspec)); # alter Wert der Variablen
          pushSTACK(varspec); # Variable
          varspecs = Cdr(varspecs);
        }
      finish_frame(DYNBIND);
    }
    # Frame fertig aufgebaut, nun die Werte der Variablen verändern:
    varspecs = *varspecs_;
    { var object* valptr = val_pointer;
      while (consp(varspecs))
        { var object varspec = Car(varspecs);
          if (consp(varspec)) { varspec = Car(varspec); }
          Symbol_value(varspec) = NEXT(valptr); # neuen Wert der Variablen zuweisen
          varspecs = Cdr(varspecs);
    }   }
    # Nun die Formen evaluieren:
    implicit_progn(*(varspecs_ STACKop -1),NIL);
    # Bindungsframe auflösen:
    unwind();
    # Stack aufräumen:
    set_args_end_pointer(val_pointer);
    skipSTACK(2);
  }}

LISPSPECFORM(progv, 2,0,body)
# (PROGV symbollist valuelist {form}), CLTL S. 112
  { STACK_2 = (eval(STACK_2),value1); # Symbolliste auswerten
   {var object valuelist = (eval(STACK_1),value1); # Wertliste auswerten
    var object body = popSTACK();
    skipSTACK(1);
    progv(popSTACK(),valuelist); # Frame aufbauen
    implicit_progn(body,NIL); # body auswerten
    unwind(); # Frame auflösen
  }}

# Fehlermeldung bei FLET/LABELS, wenn keine Funktionsspezifikation vorliegt.
# > caller: Aufrufer, ein Symbol
# > obj: fehlerhafte Funktionsspezifikation
  nonreturning_function(local, fehler_funspec, (object caller, object obj));
  local void fehler_funspec(caller,obj)
    var object caller;
    var object obj;
    { pushSTACK(obj);
      pushSTACK(caller);
      fehler(source_program_error,
             GETTEXT("~: ~ is not a function specification")
            );
    }

# UP: Beendet ein FLET/MACROLET.
# finish_flet(top_of_frame,body);
# > Stackaufbau: [top_of_frame] def1 name1 ... defn namen [STACK]
# > top_of_frame: Pointer übern Frame
# > body: Formenliste
# < mv_count/mv_space: Werte
# can trigger GC
  local Values finish_flet (object* top_of_frame, object body);
  local Values finish_flet(top_of_frame,body)
    var object* top_of_frame;
    var object body;
    {{var uintL bindcount = # Anzahl der Bindungen
        STACK_item_count(STACK,top_of_frame) / 2;
      pushSTACK(aktenv.fun_env); # aktuelles FUN_ENV als NEXT_ENV
      pushSTACK(as_object(bindcount));
      finish_frame(FUN);
     }# Funktionsbindungsframe ist fertig.
      # FENV-Bindungsframe bauen:
     {var object* top_of_frame = STACK; # Pointer übern Frame
      pushSTACK(aktenv.fun_env);
      finish_frame(ENV1F);
      # FENV-Bindungsframe ist fertig.
      # FUN_ENV erweitern:
      # top_of_frame = Pointer auf den Funktionsbindungsframe
      aktenv.fun_env = make_framepointer(top_of_frame);
     }# Formen ausführen:
      implicit_progn(body,NIL);
      unwind(); # FENV-Bindungsframe auflösen
      unwind(); # Funktionsbindungsframe auflösen
    }

LISPSPECFORM(flet, 1,0,body)
# (FLET ({funspec}) {form}), CLTL S. 113
  { var object body = popSTACK(); # ({form})
    var object funspecs = popSTACK(); # ({funspec})
    # Funktionsbindungs-Frame aufbauen:
    var object* top_of_frame = STACK; # Pointer übern Frame
    while (consp(funspecs))
      { pushSTACK(body); # Formenliste retten
        pushSTACK(Cdr(funspecs)); # restliche funspecs
        funspecs = Car(funspecs); # nächstes funspec = (name . lambdabody)
        # sollte ein Cons sein, dessen CAR ein Symbol und dessen CDR ein Cons ist:
        if (!consp(funspecs)) { fehler_spec: fehler_funspec(S(flet),funspecs); }
       {var object name = Car(funspecs);
        var object lambdabody = Cdr(funspecs);
        if (!funnamep(name)) { fehler_funsymbol(S(flet),name); }
        if (!consp(lambdabody)) { goto fehler_spec; }
        pushSTACK(name); # name retten
        # lambdabody zu einer Closure machen:
        {var object fun = get_closure(lambdabody,name,TRUE,&aktenv);
         name = popSTACK();
         funspecs = popSTACK(); # restliche funspecs
         body = popSTACK();
         # in den Frame:
         pushSTACK(fun); # als "Wert" die Closure
         pushSTACK(name); # Name, Bindung ist automatisch aktiv
      }}}
    return_Values finish_flet(top_of_frame,body);
  }

LISPSPECFORM(labels, 1,0,body)
# (LABELS ({funspec}) {form}), CLTL S. 113
  { # Auf den Aufbau eines Funktionsbindungs-Frames kann hier verzichtet werden,
    # weil bei der Bildung der ersten Closure sowieso das Environment genestet
    # und dabei dieser Funktionsbindungs-Frame in einen Vektor geschrieben würde.
    # aktuelles FUN_ENV nesten:
    pushSTACK(nest_fun(aktenv.fun_env));
    # Anzahl der funspecs bestimmen und Syntax abtesten:
   {var uintL veclength = 1; # = 2 * (Anzahl der funspecs) + 1
    { var object funspecs = STACK_(1+1);
      while (consp(funspecs))
        { var object funspec = Car(funspecs);
          # sollte ein Cons sein, dessen CAR ein Symbol und dessen CDR ein Cons ist:
          if (!consp(funspec)) { fehler_spec: fehler_funspec(S(labels),funspec); }
          {var object name = Car(funspec);
           var object lambdabody = Cdr(funspec);
           if (!funnamep(name)) { fehler_funsymbol(S(labels),name); }
           if (!consp(lambdabody)) { goto fehler_spec; }
          }
          funspecs = Cdr(funspecs);
          veclength += 2;
    }   }
    # Vektor passender Länge allozieren und darin die Namen eintragen:
    {var object vec = allocate_vector(veclength);
     {var object* ptr = &TheSvector(vec)->data[0];
      var object funspecs = STACK_(1+1);
      while (consp(funspecs))
        { *ptr++ = Car(Car(funspecs)); # nächster name
          ptr++; # Funktion bleibt vorerst NIL
          funspecs = Cdr(funspecs);
        }
      *ptr++ = popSTACK(); # genestetes FUN_ENV als letztes Vektor-Element
     }
     {var object body = popSTACK(); # Formenliste
      var object funspecs = popSTACK();
      # FENV-Bindungsframe aufbauen:
      { var object* top_of_frame = STACK; # Pointer übern Frame
        pushSTACK(aktenv.fun_env);
        finish_frame(ENV1F);
      }
      # FUN_ENV erweitern:
      aktenv.fun_env = vec;
      # Closures erzeugen und in den Vektor stecken:
      pushSTACK(body);
      pushSTACK(vec);
      {var uintL index = 1; # Index in den Vektor
       while (consp(funspecs))
         { pushSTACK(Cdr(funspecs)); # restliche funspecs
          {var object funspec = Car(funspecs);
           # Closure erzeugen:
           var object fun = get_closure(Cdr(funspec),Car(funspec),TRUE,&aktenv);
           funspecs = popSTACK();
           TheSvector(STACK_0)->data[index] = fun; # in den Vektor stecken
           index += 2;
      }  }}
      skipSTACK(1); # Vektor vergessen
      body = popSTACK();
      # Formen ausführen:
      implicit_progn(body,NIL);
      unwind(); # FENV-Bindungsframe auflösen
  }}}}

LISPSPECFORM(macrolet, 1,0,body)
# (MACROLET ({macrodef}) {form}), CLTL S. 113
  { var object body = popSTACK(); # ({form})
    var object macrodefs = popSTACK(); # ({macrodef})
    # Macrobindungs-Frame aufbauen:
    var object* top_of_frame = STACK; # Pointer übern Frame
    while (consp(macrodefs))
      { pushSTACK(body); # Formenliste retten
        pushSTACK(Cdr(macrodefs)); # restliche macrodefs
        macrodefs = Car(macrodefs); # nächstes macrodef = (name . lambdabody)
        # sollte ein Cons sein, dessen CAR ein Symbol und dessen CDR ein Cons ist:
        if (!consp(macrodefs))
          { fehler_spec:
            pushSTACK(macrodefs);
            pushSTACK(S(macrolet));
            fehler(source_program_error,
                   GETTEXT("~: ~ is not a macro specification")
                  );
          }
       {var object name = Car(macrodefs);
        if (!symbolp(name))
          { pushSTACK(name);
            pushSTACK(S(macrolet));
            fehler(source_program_error,
                   GETTEXT("~: macro name ~ should be a symbol")
                  );
          }
        if (!mconsp(Cdr(macrodefs))) { goto fehler_spec; }
        pushSTACK(name); # name retten
        # Macro-Expander bauen: (SYSTEM::MAKE-MACRO-EXPANDERCONS macrodef)
        pushSTACK(macrodefs); funcall(S(make_macro_expandercons),1);
        name = popSTACK();
        macrodefs = popSTACK(); # restliche macrodefs
        body = popSTACK();
        # in den Frame:
        pushSTACK(value1); # als "Wert" das Cons mit dem Expander
        pushSTACK(name); # Name, Bindung ist automatisch aktiv
      }}
    return_Values finish_flet(top_of_frame,body);
  }

LISPSPECFORM(symbol_macrolet, 1,0,body)
# (SYMBOL-MACROLET ({(var expansion)}) {decl} {form}), CLTL2 S. 155
  { # {decl} {form} trennen:
    var boolean to_compile = parse_dd(STACK_0,aktenv.var_env,aktenv.fun_env); # unvollständiges var_env??
    # bitte kein Docstring:
    if (!nullp(value3)) { fehler_docstring(S(symbol_macrolet),STACK_0); }
    if (to_compile) # Deklaration (COMPILE) ?
      # ja -> Form kompilieren:
      { skipSTACK(2); return_Values compile_eval_form(); }
      else
      { skipSTACK(1);
        # Variablenbindungsframe aufbauen, VAR_ENV erweitern:
       {var object* bind_ptr;
        var uintC bind_count;
        make_variable_frame(S(symbol_macrolet),popSTACK(),&bind_ptr,&bind_count);
        # Dann die Symbol-Macros bilden und die Bindungen aktivieren:
        if (bind_count > 0)
          { var object* frame_pointer = bind_ptr;
            var uintC count;
            dotimespC(count,bind_count,
              { var object* initptr = &NEXT(frame_pointer);
                var object sm = allocate_symbolmacro();
                TheSymbolmacro(sm)->symbolmacro_expansion = *initptr;
                *initptr = sm;
                frame_pointer skipSTACKop -(varframe_binding_size-1);
                Before(frame_pointer) = as_object(as_oint(Before(frame_pointer)) | wbit(active_bit_o));
              });
          }
        # Body abinterpretieren:
        implicit_progn(popSTACK(),NIL);
        # Frames auflösen:
        unwind(); # VENV-Bindungsframe auflösen
        unwind(); # Variablenbindungs-Frame auflösen
  }   }}

LISPSPECFORM(if, 2,1,nobody)
# (IF test form1 [form2]), CLTL S. 115
  { eval(STACK_2); # Bedingung auswerten
   {var object form;
    if (!nullp(value1))
      { form = STACK_1; skipSTACK(3); } # form1 auswerten
      else
      { form = STACK_0; skipSTACK(3); # form2 auswerten
        if (eq(form,unbound))
          { value1 = NIL; mv_count=1; return; } # keine angegeben -> NIL
      }
    eval(form);
  }}

LISPSPECFORM(when, 1,0,body)
# (WHEN test {form}), CLTL S. 115
  { eval(STACK_1); # Bedingung auswerten
    if (!nullp(value1))
      { var object body = STACK_0;
        skipSTACK(2);
        implicit_progn(body,NIL);
      }
      else
      { skipSTACK(2);
        value1 = NIL; mv_count=1;
      }
  }

LISPSPECFORM(unless, 1,0,body)
# (UNLESS test {form}), CLTL S. 115
  { eval(STACK_1); # Bedingung auswerten
    if (nullp(value1))
      { var object body = STACK_0;
        skipSTACK(2);
        implicit_progn(body,NIL);
      }
      else
      { skipSTACK(2);
        value1 = NIL; mv_count=1;
      }
  }

LISPSPECFORM(cond, 0,0,body)
# (COND {(bed {form})}), CLTL S. 116
  { while (mconsp(STACK_0))
      { var object clause = STACK_0; # Klausel-Liste
        STACK_0 = Cdr(clause); # restliche Klauseln retten
        clause = Car(clause); # nächste Klausel
        if (!consp(clause)) # sollte ein Cons sein
          { pushSTACK(clause);
            pushSTACK(S(cond));
            fehler(source_program_error,
                   GETTEXT("~: clause ~ should be a list")
                  );
          }
        pushSTACK(Cdr(clause)); # Klausel-Rest retten
        eval(Car(clause)); # Bedingung auswerten
        if (!nullp(value1)) goto eval_clause;
        skipSTACK(1); # nächste probieren
      }
    # keine Bedingung war erfüllt.
    skipSTACK(1); value1 = NIL; mv_count=1; return;
    # erfüllte Bedingung gefunden:
    eval_clause:
   {var object clause_rest = popSTACK(); # Klausel-Rest
    skipSTACK(1);
    implicit_progn(clause_rest,value1); # auswerten
  }}

LISPSPECFORM(case, 1,0,body)
# (CASE keyform {(keys {form})}), CLTL S. 117
  { eval(STACK_1); # keyform auswerten
   {var object value = value1;
    var object clauses = STACK_0;
    var object clause;
    skipSTACK(2);
    while (consp(clauses))
      { clause = Car(clauses); # nächste Klausel
        clauses = Cdr(clauses);
        if (!consp(clause)) # sollte ein Cons sein
          { pushSTACK(clause);
            pushSTACK(S(case));
            fehler(source_program_error,
                   GETTEXT("~: missing key list: ~")
                  );
          }
       {var object keys = Car(clause);
        if (eq(keys,T) || eq(keys,S(otherwise)))
          { if (nullp(clauses)) goto eval_clause;
            pushSTACK(keys);
            pushSTACK(S(case));
            fehler(source_program_error,
                   GETTEXT("~: the ~ clause must be the last one")
                  );
          }
        else
          { if (listp(keys))
              { while (consp(keys))
                  { if (eql(Car(keys),value)) goto eval_clause;
                    keys = Cdr(keys);
              }   }
            else
              { if (eql(keys,value)) goto eval_clause; }
          }
      }}
    # keine Bedingung war erfüllt.
    value1 = NIL; mv_count=1; return;
    # erfüllte Bedingung gefunden:
    eval_clause:
    {var object clause_rest = Cdr(clause); # Klausel-Rest
     implicit_progn(clause_rest,NIL); # auswerten
  }}}

LISPSPECFORM(block, 1,0,body)
# (BLOCK name {form}), CLTL S. 119
  { var object body = popSTACK();
    var object name = popSTACK();
    if (!symbolp(name)) { fehler_symbol(name); }
   {var sp_jmp_buf returner; # Rücksprungpunkt
    # Block-Frame aufbauen:
    { var object* top_of_frame = STACK; # Pointer übern Frame
      pushSTACK(name); # Block-Name
      pushSTACK(aktenv.block_env); # aktuelles BLOCK_ENV als NEXT_ENV
      finish_entry_frame(IBLOCK,&!returner,, goto block_return; );
    }
    # BENV-Frame aufbauen:
    {var object* top_of_frame = STACK;
     pushSTACK(aktenv.block_env);
     finish_frame(ENV1B);
    # BLOCK_ENV erweitern (top_of_frame = Pointer auf den Block-Frame)
     aktenv.block_env = make_framepointer(top_of_frame);
    }
    # Body ausführen:
    implicit_progn(body,NIL);
    unwind(); # BENV-Bindungsframe auflösen
    block_return: # Hierher wird gesprungen, wenn der BLOCK-Frame einen
                  # RETURN-FROM gefangen hat.
    unwind(); # BLOCK-Frame auflösen
  }}

# Fehler, wenn ein Block bereits verlassen wurde.
# fehler_block_left(name);
# > name: Block-Name
  nonreturning_function(global, fehler_block_left, (object name));
  global void fehler_block_left(name)
    var object name;
    { pushSTACK(name);
      pushSTACK(S(return_from));
      fehler(control_error,
             GETTEXT("~: the block named ~ has already been left")
            );
    }

LISPSPECFORM(return_from, 1,1,nobody)
# (RETURN-FROM name [result]), CLTL S. 120
  { var object name = STACK_1;
    if (!symbolp(name)) { fehler_symbol(name); } # sollte ein Symbol sein
    # BLOCK_ENV durchgehen:
   {var object env = aktenv.block_env; # aktuelles BLOCK_ENV
    var object* FRAME;
    while (framepointerp(env))
      { # env ist ein Frame-Pointer auf einen IBLOCK-Frame im Stack.
        FRAME = TheFramepointer(env);
        if (framecode(FRAME_(0)) & bit(nested_bit_t))
          # Frame schon genestet
          { env = FRAME_(frame_next_env); break; }
        if (eq(FRAME_(frame_name),name)) goto found;
        env = FRAME_(frame_next_env);
      }
    # env ist eine Aliste.
    while (consp(env))
      { var object block_cons = Car(env);
        if (eq(Car(block_cons),name))
          { env = Cdr(block_cons);
            if (eq(env,disabled)) # Block noch aktiv?
              { fehler_block_left(name); }
            goto found;
          }
        env = Cdr(env);
      }
    # env ist zu Ende.
    pushSTACK(name);
    pushSTACK(S(return_from));
    fehler(source_program_error,
           GETTEXT("~: no block named ~ is currently visible")
          );
    # Block-Frame gefunden: env
    found:
    FRAME = uTheFramepointer(env); # Pointer auf ihn
    # Werte produzieren, mit denen der Block verlassen werden soll:
    {var object result = popSTACK();
     skipSTACK(1);
     if (!eq(result,unbound)) # result angegeben?
       { eval(result); }
       else
       { value1 = NIL; mv_count=1; }
     # Zum gefundenen Block-Frame springen und ihn auflösen:
     unwind_upto(FRAME);
  }}}

# Die Funktionen MAPCAR, MAPLIST, MAPCAN, MAPCON bauen wir in zwei Versionen:
# Die erste baut die Liste im umgekehrter Reihenfolge, muss sie dann umdrehen.
# Die zweite arbeitet vorwärtsherum, braucht dafür aber ein Cons zuviel.
  #define MAP_REVERSES

#ifdef MAP_REVERSES

# Macro für MAPCAR und MAPLIST
  #define MAPCAR_MAPLIST_BODY(listaccess)  \
    { var object* args_pointer = rest_args_pointer STACKop 2;                   \
      argcount++; # argcount := Anzahl der Listen auf dem Stack                 \
      # Platz für die Funktionsaufruf-Argumente reservieren:                    \
      get_space_on_STACK(sizeof(object)*(uintL)argcount);                       \
      pushSTACK(NIL); # Anfang der Ergebnisliste                                \
     {var object* ergptr = &STACK_0; # Pointer darauf                           \
      # alle Listen parallel durchlaufen:                                       \
      loop                                                                      \
        { var object* argptr = args_pointer;                                    \
          var object fun = NEXT(argptr);                                        \
          var uintC count;                                                      \
          dotimespC(count,argcount,                                             \
            { var object* next_list_ = &NEXT(argptr);                           \
              var object next_list = *next_list_;                               \
              if (atomp(next_list)) goto fertig; # eine Liste zu Ende -> fertig \
              pushSTACK(listaccess(next_list)); # als Argument auf den Stack    \
              *next_list_ = Cdr(next_list); # Liste verkürzen                   \
            });                                                                 \
          funcall(fun,argcount); # Funktion aufrufen                            \
          pushSTACK(value1);                                                    \
         {var object new_cons = allocate_cons(); # neues Cons                   \
          Car(new_cons) = popSTACK(); Cdr(new_cons) = STACK_0;                  \
          STACK_0 = new_cons; # verlängert die Ergebnisliste                    \
        }}                                                                      \
      fertig:                                                                   \
      value1 = nreverse(*ergptr); mv_count=1; # Ergebnisliste umdrehen          \
      set_args_end_pointer(args_pointer); # STACK aufräumen                     \
    }}

#else

# Macro für MAPCAR und MAPLIST
  #define MAPCAR_MAPLIST_BODY(listaccess)  \
    { var object* args_pointer = rest_args_pointer STACKop 2;                   \
      argcount++; # argcount := Anzahl der Listen auf dem Stack                 \
      # Platz für die Funktionsaufruf-Argumente reservieren:                    \
      get_space_on_STACK(sizeof(object)*(uintL)argcount);                       \
      # Gesamtliste anfangen:                                                   \
      {var object new_cons = allocate_cons(); # (CONS NIL NIL)                  \
       pushSTACK(new_cons); # Gesamtliste                                       \
       pushSTACK(new_cons); # (last Gesamtliste)                                \
      }                                                                         \
     {var object* ergptr = &STACK_1; # Pointer darauf                           \
      # alle Listen parallel durchlaufen:                                       \
      loop                                                                      \
        { var object* argptr = args_pointer;                                    \
          var object fun = NEXT(argptr);                                        \
          var uintC count;                                                      \
          dotimespC(count,argcount,                                             \
            { var object* next_list_ = &NEXT(argptr);                           \
              var object next_list = *next_list_;                               \
              if (atomp(next_list)) goto fertig; # eine Liste zu Ende -> fertig \
              pushSTACK(listaccess(next_list)); # als Argument auf den Stack    \
              *next_list_ = Cdr(next_list); # Liste verkürzen                   \
            });                                                                 \
          funcall(fun,argcount); # Funktion aufrufen                            \
          pushSTACK(value1);                                                    \
         {var object new_cons = allocate_cons(); # neues Cons                   \
          Car(new_cons) = popSTACK(); # new_cons = (LIST (FUNCALL ...))         \
          Cdr(STACK_0) = new_cons; STACK_0 = new_cons; # verlängert Gesamtliste \
        }}                                                                      \
      fertig:                                                                   \
      value1 = Cdr(*ergptr); mv_count=1; # Ergebnisliste ohne Header-Cons       \
      set_args_end_pointer(args_pointer); # STACK aufräumen                     \
    }}

#endif

# Macro für MAPC und MAPL
  #define MAPC_MAPL_BODY(listaccess)  \
    { var object* args_pointer = rest_args_pointer STACKop 2;                   \
      argcount++; # argcount := Anzahl der Listen auf dem Stack                 \
      # Platz für die Funktionsaufruf-Argumente reservieren:                    \
      get_space_on_STACK(sizeof(object)*(uintL)argcount);                       \
      pushSTACK(BEFORE(rest_args_pointer)); # erstes Listenargument retten      \
     {var object* ergptr = &STACK_0; # Pointer darauf                           \
      # alle Listen parallel durchlaufen:                                       \
      loop                                                                      \
        { var object* argptr = args_pointer;                                    \
          var object fun = NEXT(argptr);                                        \
          var uintC count;                                                      \
          dotimespC(count,argcount,                                             \
            { var object* next_list_ = &NEXT(argptr);                           \
              var object next_list = *next_list_;                               \
              if (atomp(next_list)) goto fertig; # eine Liste zu Ende -> fertig \
              pushSTACK(listaccess(next_list)); # als Argument auf den Stack    \
              *next_list_ = Cdr(next_list); # Liste verkürzen                   \
            });                                                                 \
          funcall(fun,argcount); # Funktion aufrufen                            \
        }                                                                       \
      fertig:                                                                   \
      value1 = *ergptr; mv_count=1; # 1. Liste als Wert                         \
      set_args_end_pointer(args_pointer); # STACK aufräumen                     \
    }}

#ifdef MAP_REVERSES

# Macro für MAPCAN und MAPCON
  #define MAPCAN_MAPCON_BODY(listaccess)  \
    { var object* args_pointer = rest_args_pointer STACKop 2;                   \
      argcount++; # argcount := Anzahl der Listen auf dem Stack                 \
      # Platz für die Funktionsaufruf-Argumente reservieren:                    \
      get_space_on_STACK(sizeof(object)*(uintL)argcount);                       \
      pushSTACK(NIL); # Anfang der Ergebnisliste                                \
     {var object* ergptr = &STACK_0; # Pointer darauf                           \
      # alle Listen parallel durchlaufen:                                       \
      loop                                                                      \
        { var object* argptr = args_pointer;                                    \
          var object fun = NEXT(argptr);                                        \
          var uintC count;                                                      \
          dotimespC(count,argcount,                                             \
            { var object* next_list_ = &NEXT(argptr);                           \
              var object next_list = *next_list_;                               \
              if (atomp(next_list)) goto fertig; # eine Liste zu Ende -> fertig \
              pushSTACK(listaccess(next_list)); # als Argument auf den Stack    \
              *next_list_ = Cdr(next_list); # Liste verkürzen                   \
            });                                                                 \
          funcall(fun,argcount); # Funktion aufrufen                            \
          STACK_0 = nreconc(value1,STACK_0); # Ergebnis anhängen                \
        }                                                                       \
      fertig:                                                                   \
      value1 = nreconc(*ergptr,NIL); mv_count=1; # Ergebnisliste umdrehen       \
      set_args_end_pointer(args_pointer); # STACK aufräumen                     \
    }}

#else

# Macro für MAPCAN und MAPCON
  #define MAPCAN_MAPCON_BODY(listaccess)  \
    { var object* args_pointer = rest_args_pointer STACKop 2;                   \
      argcount++; # argcount := Anzahl der Listen auf dem Stack                 \
      # Platz für die Funktionsaufruf-Argumente reservieren:                    \
      get_space_on_STACK(sizeof(object)*(uintL)argcount);                       \
      # Gesamtliste anfangen:                                                   \
      {var object new_cons = allocate_cons(); # (CONS NIL NIL)                  \
       pushSTACK(new_cons); # Gesamtliste                                       \
       pushSTACK(new_cons); # (last Gesamtliste)                                \
      }                                                                         \
     {var object* ergptr = &STACK_1; # Pointer darauf                           \
      # alle Listen parallel durchlaufen:                                       \
      loop                                                                      \
        { var object* argptr = args_pointer;                                    \
          var object fun = NEXT(argptr);                                        \
          var uintC count;                                                      \
          dotimespC(count,argcount,                                             \
            { var object* next_list_ = &NEXT(argptr);                           \
              var object next_list = *next_list_;                               \
              if (atomp(next_list)) goto fertig; # eine Liste zu Ende -> fertig \
              pushSTACK(listaccess(next_list)); # als Argument auf den Stack    \
              *next_list_ = Cdr(next_list); # Liste verkürzen                   \
            });                                                                 \
          funcall(fun,argcount); # Funktion aufrufen                            \
         {var object list = value1; # anzuhängende Liste                        \
          if (consp(list))                                                      \
            { Cdr(STACK_0) = list; # als (cdr (last Gesamtliste)) einhängen     \
              while (mconsp(Cdr(list))) { list = Cdr(list); }                   \
              STACK_0 = list; # und (last Gesamtliste) := (last list)           \
        }}  }                                                                   \
      fertig:                                                                   \
      value1 = Cdr(*ergptr); mv_count=1; # Ergebnisliste ohne Header-Cons       \
      set_args_end_pointer(args_pointer); # STACK aufräumen                     \
    }}

#endif

#define Identity

LISPFUN(mapcar,2,0,rest,nokey,0,NIL)
# (MAPCAR fun list {list}), CLTL S. 128
  MAPCAR_MAPLIST_BODY(Car)

LISPFUN(maplist,2,0,rest,nokey,0,NIL)
# (MAPLIST fun list {list}), CLTL S. 128
  MAPCAR_MAPLIST_BODY(Identity)

LISPFUN(mapc,2,0,rest,nokey,0,NIL)
# (MAPC fun list {list}), CLTL S. 128
  MAPC_MAPL_BODY(Car)

LISPFUN(mapl,2,0,rest,nokey,0,NIL)
# (MAPL fun list {list}), CLTL S. 128
  MAPC_MAPL_BODY(Identity)

LISPFUN(mapcan,2,0,rest,nokey,0,NIL)
# (MAPCAN fun list {list}), CLTL S. 128
  MAPCAN_MAPCON_BODY(Car)

LISPFUN(mapcon,2,0,rest,nokey,0,NIL)
# (MAPCON fun list {list}), CLTL S. 128
  MAPCAN_MAPCON_BODY(Identity)

LISPSPECFORM(tagbody, 0,0,body)
# (TAGBODY {tag | statement}), CLTL S. 130
  { var object body = popSTACK();
    # GENV-Frame aufbauen:
    { var object* top_of_frame = STACK; # Pointer übern Frame
      pushSTACK(aktenv.go_env);
      finish_frame(ENV1G);
    }
    # TAGBODY-Frame aufbauen:
   {var object* top_of_frame = STACK; # Pointer übern Frame
    # Body durchparsen und Tags im Stack ablegen:
    var uintL tagcount = 0;
    { var object body_rest = body;
      while (consp(body_rest))
        { var object item = Car(body_rest);
          body_rest = Cdr(body_rest);
          # Als Tags werden Symbole sowie Zahlen angesehen
          # (wie im Compiler), Conses sind Statements.
          if (atomp(item))
            { if (numberp(item) || symbolp(item))
                # Marke im Stack ablegen:
                { check_STACK();
                  pushSTACK(body_rest); # Body-Rest nach der Marke
                  pushSTACK(item);
                  tagcount++;
                }
                else
                { pushSTACK(item);
                  pushSTACK(S(tagbody));
                  fehler(source_program_error,
                         GETTEXT("~: ~ is neither tag nor form")
                        );
                }
    }   }   }
    if (tagcount>0)
      { var sp_jmp_buf returner; # Rücksprungpunkt
        pushSTACK(aktenv.go_env); # aktuelles GO_ENV als NEXT_ENV
        finish_entry_frame(ITAGBODY,&!returner,, goto go_entry; );
        # GO_ENV erweitern:
        aktenv.go_env = make_framepointer(STACK);
        if (FALSE)
          { go_entry: # Hierher wird gesprungen, wenn dieser Frame ein GO
                      # gefangen hat.
            body = value1; # Die Formenliste wird als value1 übergeben.
          }
        # Statements abarbeiten:
        pushSTACK(body);
        while (mconsp(STACK_0))
          { var object body_rest = STACK_0;
            STACK_0 = Cdr(body_rest); # restlicher Body
            body_rest = Car(body_rest); # nächstes Item
            if (consp(body_rest)) { eval(body_rest); } # Form -> auswerten
          }
        skipSTACK(1); # Body vergessen
        unwind(); # TAGBODY-Frame auflösen
        unwind(); # GENV-Frame auflösen
      }
      else
      # Body ohne Tags -> nur PROGN mit Wert NIL
      { skipSTACK(2); # GENV-Frame wieder auflösen, GENV ist unverändert
        pushSTACK(body); implicit_prog();
      }
    value1 = NIL; mv_count=1; # Wert NIL
  }}

LISPSPECFORM(go, 1,0,nobody)
# (GO tag), CLTL S. 133
  { var object tag = popSTACK();
    if (!(numberp(tag) || symbolp(tag)))
      { pushSTACK(tag);
        pushSTACK(S(go));
        fehler(source_program_error,
               GETTEXT("~: illegal tag ~")
              );
      }
    # GO_ENV durchgehen:
   {var object env = aktenv.go_env; # aktuelles GO_ENV
    var object* FRAME;
    while (framepointerp(env))
      { # env ist ein Frame-Pointer auf einen ITAGBODY-Frame im Stack.
        FRAME = uTheFramepointer(env);
        if (framecode(FRAME_(0)) & bit(nested_bit_t))
          # Frame schon genestet
          { env = FRAME_(frame_next_env); break; }
        # Tags im ungenesteten ITAGBODY-Frame absuchen:
        { var object* bind_ptr = &FRAME_(frame_bindings); # Pointer unter die Tagbindungen
          var object* bindend_ptr = STACKpointable(topofframe(FRAME_(0))); # Pointer über die Tagbindungen
          do { if (eql(*bind_ptr,tag)) # Tag gefunden?
                 { value1 = *(bind_ptr STACKop 1); # Formenliste aus dem Frame holen
                   goto found;
                 }
               bind_ptr skipSTACKop 2;
             }
             until (bind_ptr==bindend_ptr);
        }
        env = FRAME_(frame_next_env);
      }
    # env ist eine Aliste.
    while (consp(env))
      { var object tagbody_cons = Car(env);
        var object tagbody_vec = Car(tagbody_cons); # Tag-Vektor
        var object* tagptr = &TheSvector(tagbody_vec)->data[0];
        var uintL index = 0;
        var uintL count;
        dotimespL(count,Svector_length(tagbody_vec),
          { if (eql(*tagptr++,tag)) # Tag gefunden?
              { env = Cdr(tagbody_cons);
                if (eq(env,disabled)) # Tagbody noch aktiv?
                  { pushSTACK(tag);
                    pushSTACK(S(go));
                    fehler(control_error,
                           GETTEXT("~: tagbody for tag ~ has already been left")
                          );
                  }
                FRAME = uTheFramepointer(env); # Pointer auf den (noch aktiven!) Frame
                value1 = FRAME_(frame_bindings+2*index+1); # Formenliste
                goto found;
              }
            index++;
          });
        env = Cdr(env);
      }
    # env ist zu Ende.
    pushSTACK(tag);
    pushSTACK(S(go));
    fehler(source_program_error,
           GETTEXT("~: no tag named ~ is currently visible")
          );
    # Tagbody-Frame gefunden. FRAME ist ein Pointer auf ihn (ohne Typinfo),
    # value1 die Liste der auszuführenden Formen.
    found:
    mv_count=1; # Formenliste value1 wird übergeben
    # Zum gefundenen Tagbody-Frame springen und dort weitermachen:
    unwind_upto(FRAME);
  }}

# Fehlermeldung bei zu vielen Werten
# fehler_mv_zuviel(caller);
# > caller: Aufrufer, ein Symbol
  nonreturning_function(global, fehler_mv_zuviel, (object caller));
  global void fehler_mv_zuviel(caller)
    var object caller;
    { pushSTACK(caller);
      fehler(error,
             GETTEXT("~: too many values")
            );
    }

LISPFUN(values,0,0,rest,nokey,0,NIL)
# (VALUES {arg}), CLTL S. 134
  { if (argcount >= mv_limit) { fehler_mv_zuviel(S(values)); }
    STACK_to_mv(argcount);
  }

LISPFUNN(values_list,1)
# (VALUES-LIST list), CLTL S. 135
  { list_to_mv(popSTACK(), fehler_mv_zuviel(S(values_list)); ); }

LISPSPECFORM(multiple_value_list, 1,0,nobody)
# (MULTIPLE-VALUE-LIST form), CLTL S. 135
  { eval(popSTACK()); # form auswerten
    mv_to_list(); # Werte in Liste packen
    value1 = popSTACK(); mv_count=1; # Liste als Wert
  }

LISPSPECFORM(multiple_value_call, 1,0,body)
# (MULTIPLE-VALUE-CALL fun {form}), CLTL S. 135
  { var object* fun_ = &STACK_1;
    *fun_ = (eval(*fun_),value1); # Funktion auswerten
   {var object forms = popSTACK(); # Formenliste
    var uintL argcount = 0; # Anzahl der bisherigen Argumente
    while (consp(forms))
      { pushSTACK(Cdr(forms)); # restliche Formen
        eval(Car(forms)); # nächste Form auswerten
        forms = popSTACK();
        # Deren Werte in den Stack:
        argcount += (uintL)mv_count;
        mv_to_STACK();
      }
    if (((uintL)~(uintL)0 > ca_limit_1) && (argcount > ca_limit_1))
      { pushSTACK(*fun_);
        pushSTACK(S(multiple_value_call));
        fehler(program_error,
               GETTEXT("~: too many arguments to ~")
              );
      }
    funcall(*fun_,argcount); # Funktion aufrufen
    skipSTACK(1);
  }}

LISPSPECFORM(multiple_value_prog1, 1,0,body)
# (MULTIPLE-VALUE-PROG1 form {form}), CLTL S. 136
  {  eval(STACK_1); # erste Form auswerten
   { var object body = popSTACK();
     skipSTACK(1);
    {var uintC mvcount = mv_count; # Wertezahl
     mv_to_STACK(); # alle Werte in den Stack
     pushSTACK(body); implicit_prog();
     STACK_to_mv(mvcount); # alle Werte wieder aus dem Stack zurückholen
  }}}

LISPSPECFORM(multiple_value_bind, 2,0,body)
# (MULTIPLE-VALUE-BIND ({var}) values-form {decl} {form}), CLTL S. 136
  { # {decl} {form} trennen:
    var boolean to_compile = parse_dd(STACK_0,aktenv.var_env,aktenv.fun_env); # unvollständiges var_env??
    # bitte kein Docstring:
    if (!nullp(value3)) { fehler_docstring(S(multiple_value_bind),STACK_0); }
    if (to_compile) # Deklaration (COMPILE) ?
      # ja -> Form kompilieren:
      { skipSTACK(3); return_Values compile_eval_form(); }
      else
      { var object varlist = STACK_2;
        STACK_2 = STACK_1;
        skipSTACK(2);
        # Variablenbindungsframe aufbauen, VAR_ENV erweitern:
       {var object* form_ = &STACK_0;
        var object* bind_ptr;
        var uintC bind_count;
        make_variable_frame(S(multiple_value_bind),varlist,&bind_ptr,&bind_count);
        # Stackaufbau: values-form, Variablenbindungsframe, Env-Bindungs-Frame, ({form}).
        # Dann values-form auswerten:
        eval(*form_);
        # Macro zum Binden von Variablen im Variablenframe:
        # Bindet die nächste Variable an value, erniedrigt frame_pointer um 2 bzw. 3.
        #define bind_next_var(value)  \
          { var object* valptr = &Next(frame_pointer);                        \
            frame_pointer skipSTACKop -varframe_binding_size;                 \
           {var object* markptr = &Before(frame_pointer);                     \
            if (as_oint(*markptr) & wbit(dynam_bit_o))                        \
              # dynamische Bindung aktivieren:                                \
              { var object sym = *(markptr STACKop varframe_binding_sym); # Variable     \
                *valptr = TheSymbolflagged(sym)->symvalue; # alten Wert in den Frame     \
                *markptr = as_object(as_oint(*markptr) | wbit(active_bit_o)); # Bindung aktivieren \
                TheSymbolflagged(sym)->symvalue = (value); # neuen Wert in die Wertzelle \
              }                                                               \
              else                                                            \
              # statische Bindung aktivieren:                                 \
              { *valptr = (value); # neuen Wert in den Frame                  \
                *markptr = as_object(as_oint(*markptr) | wbit(active_bit_o)); # Bindung aktivieren \
              }                                                               \
          }}
        # Die r:=bind_count Variablen an die s:=mv_count Werte binden:
        # (Falls die Variablen ausgehen: restliche Werte wegwerfen;
        #  falls die Werte ausgehen: mit NIL auffüllen.)
        # Hier r>=0 und s>=0.
        { var object* frame_pointer = bind_ptr;
          var uintC r = bind_count;
          var object* mv_pointer;
          var uintC s = mv_count;
          if (r==0) goto ok; # keine Variablen?
          if (s==0) goto fill; # keine Werte?
          # noch min(r,s)>0 Werte binden:
          #if !defined(VALUE1_EXTRA)
          mv_pointer = &mv_space[0];
          #else
          bind_next_var(value1);
          if (--r == 0) goto ok; # keine Variablen mehr?
          if (--s == 0) goto fill; # keine Werte mehr?
          mv_pointer = &mv_space[1];
          #endif
          # noch min(r,s)>0 Werte binden:
          loop
            { bind_next_var(*mv_pointer++);
              if (--r == 0) goto ok; # keine Variablen mehr?
              if (--s == 0) goto fill; # keine Werte mehr?
            }
          fill: # Noch r>0 Variablen an NIL binden
          dotimespC(r,r, { bind_next_var(NIL); } );
          ok: ;
        }
        # Body abinterpretieren:
        implicit_progn(popSTACK(),NIL);
        # Frames auflösen:
        unwind(); # VENV-Bindungsframe auflösen
        unwind(); # Variablenbindungs-Frame auflösen
        skipSTACK(1);
  }   }}

LISPSPECFORM(multiple_value_setq, 2,0,nobody)
# (MULTIPLE-VALUE-SETQ ({var}) form), CLTL S. 136
  { {var object varlist = STACK_1;
     # Variablenliste durchgehen:
     while (consp(varlist))
       { var object symbol = Car(varlist); # nächste Variable
         if (!symbolp(symbol)) # sollte ein Symbol
           { fehler_kein_symbol(S(multiple_value_setq),symbol); }
         if (constantp(TheSymbol(symbol))) # und keine Konstante sein
           { fehler_symbol_constant(S(multiple_value_setq),symbol); }
         if (sym_macrop(symbol)) # und kein Symbol-Macro
           goto expand;
         varlist = Cdr(varlist);
    }  }
    if (FALSE)
      { expand:
        pushSTACK(STACK_0); STACK_1 = STACK_2; STACK_2 = S(multiple_value_setf);
       {var object newform = listof(3); # aus MULTIPLE-VALUE-SETQ mache MULTIPLE-VALUE-SETF
        eval(newform);
      }}
      else
      {  eval(popSTACK()); # form auswerten
       { var object varlist = popSTACK();
         var object* args_end = args_end_pointer;
         mv_to_STACK(); # Werte in den Stack schreiben (erleichtert den Zugriff)
         # Variablenliste durchgehen:
        {var object* mvptr = args_end;
         var uintC count = mv_count; # Anzahl noch verfügbarer Werte
         while (consp(varlist))
           { var object value;
             if (count>0)
               { value = NEXT(mvptr); count--; } # nächster Wert
               else
               { value = NIL; } # NIL, wenn alle Werte verbraucht
             setq(Car(varlist),value); # der nächsten Variablen zuweisen
             varlist = Cdr(varlist);
           }
         set_args_end_pointer(args_end); # STACK aufräumen
         mv_count=1; # letzter value1 als einziger Wert
  }   }}}

LISPSPECFORM(catch, 1,0,body)
# (CATCH tag {form}), CLTL S. 139
  { STACK_1 = (eval(STACK_1),value1); # tag auswerten
    # CATCH-Frame zu Ende aufbauen:
   {var object body = popSTACK(); # ({form})
    var object* top_of_frame = STACK STACKop 1; # Pointer übern Frame
    var sp_jmp_buf returner; # Rücksprungpunkt merken
    finish_entry_frame(CATCH,&!returner,, goto catch_return; );
    # Body ausführen:
    implicit_progn(body,NIL);
    catch_return: # Hierher wird gesprungen, wenn der oben aufgebaute
                  # Catch-Frame einen Throw gefangen hat.
    skipSTACK(3); # CATCH-Frame auflösen
  }}

LISPSPECFORM(unwind_protect, 1,0,body)
# (UNWIND-PROTECT form {cleanup}), CLTL S. 140
  { var object cleanup = popSTACK();
    var object form = popSTACK();
    # UNWIND-PROTECT-Frame aufbauen:
    pushSTACK(cleanup);
   {var object* top_of_frame = STACK;
    var sp_jmp_buf returner; # Rücksprungpunkt
    finish_entry_frame(UNWIND_PROTECT,&!returner,, goto throw_save; );
    # Protected form auswerten:
    eval(form);
    # Cleanup nach normaler Beendigung der Protected form:
      # UNWIND-PROTECT-Frame auflösen:
      skipSTACK(2);
      cleanup = popSTACK();
      # Werte retten:
     {var uintC mvcount = mv_count;
      mv_to_STACK();
      # Cleanup-Formen abarbeiten:
      pushSTACK(cleanup); implicit_prog();
      # Werte zurückschreiben:
      STACK_to_mv(mvcount);
     }
    return;
    throw_save: # Hierher wird gesprungen, wenn der oben aufgebaute
                # Unwind-Protect-Frame einen Throw aufgehalten hat.
                # unwind_protect_to_save ist zu retten und am Schluss anzuspringen.
    { var restart fun = unwind_protect_to_save.fun;
      var object* arg = unwind_protect_to_save.upto_frame;
    # Cleanup:
      # UNWIND-PROTECT-Frame auflösen:
      skipSTACK(2);
      cleanup = popSTACK();
      # Werte retten:
     {var uintC mvcount = mv_count;
      mv_to_STACK();
      # Cleanup-Formen abarbeiten:
      pushSTACK(cleanup); implicit_prog();
      # Werte zurückschreiben:
      STACK_to_mv(mvcount);
     }# und weiterspringen:
      fun(arg);
    }
  }}

LISPSPECFORM(throw, 2,0,nobody)
# (THROW tag result), CLTL S. 142
  { STACK_1 = (eval(STACK_1),value1); # tag auswerten
    eval(popSTACK()); # result auswerten
   {var object tag = popSTACK(); # ausgewertetes Tag
    throw_to(tag); # versuche auf dieses zu THROWen
    # Nicht gelungen.
    pushSTACK(tag);
    pushSTACK(S(throw));
    fehler(control_error,
           GETTEXT("~: there is no CATCHer for tag ~")
          );
  }}

LISPFUNN(driver,1)
# (SYS::DRIVER fun) baut einen Driver-Frame auf, der jedesmal die Funktion
# fun (mit 0 Argumenten) aufruft. fun wird in einer Endlosschleife ausgeführt,
# die mit GO oder THROW abgebrochen werden kann.
  { var object* top_of_frame = STACK; # Pointer übern Frame
    var sp_jmp_buf returner; # Einsprungpunkt merken
    finish_entry_frame(DRIVER,&!returner,,;);
    # Hier ist der Einsprungpunkt.
    loop { funcall(STACK_(0+2),0); } # fun aufrufen, Endlosschleife
  }

LISPFUNN(unwind_to_driver,0)
# (SYS::UNWIND-TO-DRIVER) macht ein UNWIND bis zum nächsthöheren Driver-Frame.
  { reset(); }

# Überprüft ein optionales Macroexpansions-Environment in STACK_0.
# > STACK_0: Argument
# < STACK_0: Macroexpansions-Environment #(venv fenv)
# can trigger GC
  local void test_env (void);
  local void test_env()
    { var object arg = STACK_0;
      if (eq(arg,unbound)
          || nullp(arg) # required by ANSI CL sections 3.1.1.3.1, 3.1.1.4
         )
        { STACK_0 = allocate_vector(2); } # Vektor #(nil nil) als Default
      elif (!(simple_vector_p(arg) && (Svector_length(arg) == 2)))
        { pushSTACK(arg); # Wert für Slot DATUM von TYPE-ERROR
          pushSTACK(O(type_svector2)); # Wert für Slot EXPECTED-TYPE von TYPE-ERROR
          pushSTACK(arg);
          fehler(type_error,
                 GETTEXT("Argument ~ is not a macroexpansion environment")
                );
    }   }

LISPFUN(macro_function,1,1,norest,nokey,0,NIL)
# (MACRO-FUNCTION symbol [env]), CLTL S. 144; Issue MACRO-FUNCTION-ENVIRONMENT:YES
  { test_env();
   {var object env = popSTACK();
    var object symbol = popSTACK();
    if (!symbolp(symbol)) { fehler_symbol(symbol); }
    {var object fundef = sym_function(symbol,TheSvector(env)->data[1]);
     if (fsubrp(fundef))
       # ein FSUBR -> Propertyliste absuchen: (GET symbol 'SYS::MACRO)
       { var object got = get(symbol,S(macro)); # suchen
         if (eq(got,unbound)) goto nil; # nichts gefunden?
         value1 = got;
       }
     elif (consp(fundef) && eq(Car(fundef),S(macro))) # (SYS::MACRO . expander) ?
       { value1 = Cdr(fundef); }
     else # SUBR/Closure/#<UNBOUND> -> keine Macrodefinition
       { nil: value1 = NIL; }
     mv_count=1;
  }}}

LISPFUNN(old_macro_function,1)
# (SYS::OLD-MACRO-FUNCTION symbol), for backward compatibility
  { pushSTACK(unbound); C_macro_function(); }

LISPFUN(macroexpand,1,1,norest,nokey,0,NIL)
# (MACROEXPAND form [env]), CLTL S. 151
  { test_env();
   {var object env = popSTACK();
    var object form = STACK_0;
    STACK_0 = env; # env retten
    macroexp0(form,env); # expandieren
    if (!nullp(value2)) # was getan?
      # ja -> zu Tode expandieren:
      { do { macroexp0(value1,STACK_0); } until (nullp(value2));
        value2 = T;
      }
    mv_count=2; skipSTACK(1);
  }}

LISPFUN(macroexpand_1,1,1,norest,nokey,0,NIL)
# (MACROEXPAND-1 form [env]), CLTL S. 151
  { test_env();
   {var object env = popSTACK();
    var object form = popSTACK();
    macroexp0(form,env); # 1 mal expandieren
    mv_count=2;
  }}

LISPSPECFORM(declare, 0,0,body)
# (DECLARE {decl-spec}), CLTL S. 153
  { # ({decl-spec}) bereits in STACK_0
    fehler(source_program_error,
           GETTEXT("declarations ~ are not allowed here")
          );
  }

LISPSPECFORM(the, 2,0,nobody)
# (THE value-type form), CLTL S. 161
  { eval(STACK_0); # form auswerten
    mv_to_list(); # Werteliste bilden und retten
    # Stackaufbau: value-type, form, values.
    # zum Typ-Check (SYS::%THE values (SYS::TYPE-FOR-DISCRIMINATION value-type))
    # aufrufen:
    pushSTACK(STACK_0);
    pushSTACK(STACK_(2+1)); funcall(S(type_for_discrimination),1); pushSTACK(value1);
    funcall(S(pthe),2);
    if (nullp(value1))
      # Typ-Check misslang
      { pushSTACK(STACK_(2+0)); # value-type
        pushSTACK(STACK_(0+1)); # values
        pushSTACK(STACK_(1+2)); # form
        pushSTACK(S(the));
        fehler(error, # type_error ??
               GETTEXT("~: ~ evaluated to the values ~, not of type ~")
              );
      }
    # Typ-Check OK -> Werte zurückgeben:
    list_to_mv(popSTACK(), { fehler_mv_zuviel(S(the)); } );
    skipSTACK(2);
  }

LISPFUNN(proclaim,1)
# (PROCLAIM decl-spec)
  { var object declspec = popSTACK();
    if (!consp(declspec))
      { pushSTACK(declspec);
        pushSTACK(S(proclaim));
        fehler(error,
               GETTEXT("~: bad declaration ~")
              );
      }
   {var object decltype = Car(declspec); # Deklarationstyp
    if (eq(decltype,S(special))) # SPECIAL
      { while (consp( declspec = Cdr(declspec) ))
          { var object symbol = Car(declspec);
            if (!symbolp(symbol)) { fehler_symbol(symbol); }
            if (!keywordp(symbol)) { clear_const_flag(TheSymbol(symbol)); }
            set_special_flag(TheSymbol(symbol));
      }   }
    elif (eq(decltype,S(declaration))) # DECLARATION
      { while (consp( declspec = Cdr(declspec) ))
          { var object symbol = Car(declspec);
            if (!symbolp(symbol)) { fehler_symbol(symbol); }
            # (PUSHNEW symbol (cdr declaration-types)) :
            { var object list = Cdr(O(declaration_types));
              while (consp(list))
                { if (eq(Car(list),symbol)) goto not_adjoin;
                  list = Cdr(list);
            }   }
            pushSTACK(declspec); pushSTACK(symbol);
           {var object new_cons = allocate_cons();
            var object list = O(declaration_types);
            Car(new_cons) = popSTACK(); Cdr(new_cons) = Cdr(list);
            Cdr(list) = new_cons;
            declspec = popSTACK();
           }
            not_adjoin: ;
      }   }
    elif (eq(decltype,S(inline)) || eq(decltype,S(notinline))) # INLINE, NOTINLINE
      { pushSTACK(decltype);
        while (consp( declspec = Cdr(declspec) ))
          { var object symbol = Car(declspec);
            if (!funnamep(symbol)) { fehler_kein_symbol(S(proclaim),symbol); }
            # (SYS::%PUT (SYS::GET-FUNNAME-SYMBOL symbol) 'SYS::INLINABLE decltype) :
            pushSTACK(declspec);
            pushSTACK(symbol); funcall(S(get_funname_symbol),1); pushSTACK(value1);
            pushSTACK(S(inlinable));
            pushSTACK(STACK_(1+2));
            funcall(L(put),3);
            declspec = popSTACK();
          }
        skipSTACK(1);
      }
    elif (eq(decltype,S(constant_inline)) || eq(decltype,S(constant_notinline))) # CONSTANT-INLINE, CONSTANT-NOTINLINE
      { pushSTACK(decltype);
        while (consp( declspec = Cdr(declspec) ))
          { var object symbol = Car(declspec);
            if (!symbolp(symbol)) { fehler_kein_symbol(S(proclaim),symbol); }
            # (SYS::%PUT symbol 'SYS::CONSTANT-INLINABLE decltype) :
            pushSTACK(declspec);
            pushSTACK(symbol); pushSTACK(S(constant_inlinable)); pushSTACK(STACK_(1+2)); funcall(L(put),3);
            declspec = popSTACK();
          }
        skipSTACK(1);
      }
    # Alles restliche wird ignoriert.
    value1 = NIL; mv_count=1;
  }}

LISPFUNN(eval,1)
# (EVAL form), CLTL S. 321
  { eval_noenv(popSTACK()); } # form im leeren Environment auswerten

LISPSPECFORM(load_time_value, 1,1,nobody)
# (LOAD-TIME-VALUE form [read-only-p]), CLTL2 S. 680
  { var object form = STACK_1;
    skipSTACK(2); # read-only-p ignorieren
    eval_noenv(form); # form im leeren Environment auswerten
    mv_count=1;
  }

# UP: Überprüft ein optionales Environment-Argument für EVALHOOK und APPLYHOOK.
# test_optional_env_arg(&env5);
# > subr_self: Aufrufer (ein SUBR)
# < env5: 5 Komponenten des Environments
# erhöht STACK um 1
  local void test_optional_env_arg (environment* env5);
  local void test_optional_env_arg(env5)
    var environment* env5;
    { var object env = popSTACK(); # env-Argument
      if (eq(env,unbound)) # nicht angegeben -> leeres Environment
        { env5->var_env   = NIL;
          env5->fun_env   = NIL;
          env5->block_env = NIL;
          env5->go_env    = NIL;
          env5->decl_env  = O(top_decl_env);
        }
      elif (simple_vector_p(env) && (Svector_length(env) == 5))
        # ein Simple-Vector der Länge 5
        { env5->var_env   = TheSvector(env)->data[0];
          env5->fun_env   = TheSvector(env)->data[1];
          env5->block_env = TheSvector(env)->data[2];
          env5->go_env    = TheSvector(env)->data[3];
          env5->decl_env  = TheSvector(env)->data[4];
        }
      else
        { pushSTACK(env); # Wert für Slot DATUM von TYPE-ERROR
          pushSTACK(O(type_svector5)); # Wert für Slot EXPECTED-TYPE von TYPE-ERROR
          pushSTACK(env);
          pushSTACK(TheSubr(subr_self)->name);
          fehler(type_error,
                 GETTEXT("~: ~ may not be used as an environment")
                );
    }   }

LISPFUN(evalhook,3,1,norest,nokey,0,NIL)
# (EVALHOOK form evalhookfn applyhookfn [env]), CLTL S. 323
  { var environment env5;
    test_optional_env_arg(&env5); # env-Argument nach env5
   {var object applyhookfn = popSTACK();
    var object evalhookfn = popSTACK();
    var object form = popSTACK();
    bindhooks(evalhookfn,applyhookfn); # *EVALHOOK* und *APPLYHOOK* binden
    # Environment-Frame aufbauen:
    make_ENV5_frame();
    # aktuelle Environments setzen:
    aktenv = env5;
    # form unter Umgehung von *EVALHOOK* und *APPLYHOOK* auswerten:
    eval_no_hooks(form);
    unwind(); # Environment-Frame auflösen
    unwind(); # Bindungsframe für *EVALHOOK* / *APPLYHOOK* auflösen
  }}

LISPFUN(applyhook,4,1,norest,nokey,0,NIL)
# (APPLYHOOK function args evalhookfn applyhookfn [env]), CLTL S. 323
  { var environment env5;
    test_optional_env_arg(&env5); # env-Argument nach env5
   {var object applyhookfn = popSTACK();
    var object evalhookfn = popSTACK();
    var object args = popSTACK();
    var object fun = popSTACK();
    bindhooks(evalhookfn,applyhookfn); # *EVALHOOK* und *APPLYHOOK* binden
    # Environment-Frame aufbauen:
    make_ENV5_frame();
    # aktuelle Environments setzen:
    aktenv = env5;
    # fun retten:
    { pushSTACK(fun);
     {var object* fun_ = &STACK_0;
      # Argumente einzeln auswerten und auf dem Stack ablegen:
      var uintC argcount = 0;
      while (consp(args))
        { pushSTACK(Cdr(args)); # restliche Argumentliste
          eval_no_hooks(Car(args)); # nächstes arg auswerten
          args = STACK_0; STACK_0 = value1; # Wert im Stack ablegen
          argcount++;
          if (argcount==0) # Überlauf?
            { pushSTACK(*fun_);
              pushSTACK(S(applyhook));
              fehler(program_error,
                     GETTEXT("~: too many arguments given to ~")
                    );
        }   }
      funcall(*fun_,argcount); # Funktion anwenden
      skipSTACK(1);
    }}
    unwind(); # Environment-Frame auflösen
    unwind(); # Bindungsframe für *EVALHOOK* / *APPLYHOOK* auflösen
  }}

LISPFUNN(constantp,1)
# (CONSTANTP expr), CLTL S. 324
  { var object arg = popSTACK();
    #ifdef TYPECODES
    switch (typecode(arg))
    #else
    if (orecordp(arg))
      switch (Record_type(arg))
        { case_Rectype_Symbol_above;
          case_Rectype_number_above;
          case_Rectype_array_above;
          default:
            goto nein;
        }
    elif (consp(arg)) { goto case_cons; }
    elif (charp(arg)) { goto case_char; }
    elif (immediate_number_p(arg)) { goto case_number; }
    else switch (0)
    #endif
      { case_cons: # Cons
          if (eq(Car(arg),S(quote))) goto ja; else goto nein;
        case_symbol: # Symbol
          if (constantp(TheSymbol(arg))) goto ja; else goto nein;
        case_number: # Zahl
        case_char: # Character
        case_array: # Array
          goto ja;
        default:
          goto nein;
      }
    ja: value1 = T; mv_count=1; return;
    nein: value1 = NIL; mv_count=1; return;
  }

LISPFUNN(function_name_p,1)
# (SYS::FUNCTION-NAME-P expr) erkennt Funktionsnamen
  { var object arg = popSTACK();
    value1 = (funnamep(arg) ? T : NIL); mv_count=1;
  }

LISPFUN(parse_body,1,2,norest,nokey,0,NIL)
# (SYS::PARSE-BODY body [docstring-allowed [env]])
# parst body, erkennt Deklarationen, liefert 3 Werte:
# 1. body-rest, alle Formen nach den Deklarationen,
# 2. Liste der aufgetretenen declspecs
# 3. docstring (nur falls docstring-allowed=T war) oder NIL.
# (docstring-allowed sollte = NIL oder T sein,
#  env sollte ein Function-Environment sein.)
  { test_env();
   {var boolean docstring_allowed = (!eq(STACK_1,unbound) && !nullp(STACK_1)); # Docstrings erlaubt?
    var object body = STACK_2; # body = ({decl|doc} {form})
    STACK_1 = NIL; # Noch war kein Doc-String da
    pushSTACK(NIL); # Anfang decl-spec-Liste
    # Stackaufbau: body, docstring, env, declspecs.
    while (consp(body))
      {  pushSTACK(body); # body retten
       { var object form = Car(body); # nächste Form
         # evtl. macroexpandieren (ohne FSUBRs, Symbole zu expandieren):
         do { var object env = STACK_(1+1);
              macroexp(form,TheSvector(env)->data[0],TheSvector(env)->data[1]);
              form = value1;
            }
            until (nullp(value2));
         body = popSTACK();
        {var object body_rest = Cdr(body); # body verkürzen
         if (stringp(form)) # Doc-String gefunden?
           { if (atomp(body_rest)) # an letzter Stelle der Formenliste?
               goto fertig; # ja -> letzte Form kann kein Doc-String sein!
             if (!docstring_allowed) # kein Doc-String erlaubt?
               { pushSTACK(STACK_3); # ganzer body
                 fehler(source_program_error,
                        GETTEXT("no doc-strings allowed here: ~")
                       );
               }
             if (!nullp(STACK_2)) # schon ein Doc-String dagewesen?
               # ja -> mehr als ein Doc-String ist zuviel:
               { pushSTACK(STACK_3); # ganzer body
                 fehler(source_program_error,
                        GETTEXT("Too many documentation strings in ~")
                       );
               }
             STACK_2 = form; # neuer Doc-String
             body = body_rest;
           }
         elif (consp(form) && eq(Car(form),S(declare))) # Deklaration (DECLARE ...) ?
           { # neue decl-specs einzeln auf STACK_0 consen:
             pushSTACK(body_rest); # body_rest retten
             pushSTACK(Cdr(form)); # Liste der neuen decl-specs
             while (mconsp(STACK_0))
               { # Diese Deklaration auf STACK_(0+2) consen:
                 var object new_cons = allocate_cons();
                 Car(new_cons) = Car(STACK_0);
                 Cdr(new_cons) = STACK_(0+2);
                 STACK_(0+2) = new_cons;
                 # zum nächsten decl-spec:
                 STACK_0 = Cdr(STACK_0);
               }
             skipSTACK(1);
             body = popSTACK(); # body := alter body_rest
           }
         else
           { fertig: # fertig mit Durchlaufen der Formenliste
             #if 0 # Im Interpreter zwar eine gute Idee, aber der Compiler
                   # wird dadurch behindert, weil er dann CASE und HANDLER-BIND
                   # nicht so gut compilieren kann.
             if (!eq(form,Car(body))) # Sofern die Form expandiert wurde,
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
    skipSTACK(1);
    value3 = popSTACK(); # Doc-String
    skipSTACK(1);
    mv_count=3; # 3 Werte: ({form}), declspecs, doc
  }}

LISPFUNN(keyword_test,2)
# (SYSTEM::KEYWORD-TEST arglist kwlist)
# stellt fest, ob in der Argumentliste arglist (eine paarige Keyword/Value -
# Liste) alle Keywords in der Liste kwlist  vorkommen oder aber
# ein Keyword/Value-Paar :ALLOW-OTHER-KEYS mit value /= NIL vorkommt.
# Wenn nein, Error.
  { var object arglist = STACK_1;
    # Argumente-Zahl überprüfen:
    { var uintL argcount = llength(arglist);
      if (!((argcount%2) == 0))
        { pushSTACK(arglist);
          fehler(program_error,
                 GETTEXT("keyword argument list ~ has an odd length")
                );
    }   }
    # Suche, ob :ALLOW-OTHER-KEYS kommt:
    { var object arglistr = arglist;
      while (consp(arglistr))
        { if (eq(Car(arglistr),S(Kallow_other_keys)) && !nullp(Car(Cdr(arglistr))))
            goto fertig;
          arglistr = Cdr(Cdr(arglistr));
    }   }
    # Suche, ob alle angegebenen Keywords in kwlist vorkommen:
    { var object arglistr = arglist;
      while (consp(arglistr))
        { var object key = Car(arglistr);
          var object kwlistr = STACK_0;
          while (consp(kwlistr))
            { if (eq(Car(kwlistr),key)) goto found;
              kwlistr = Cdr(kwlistr);
            }
          # nicht gefunden
          pushSTACK(key); # Wert für Slot DATUM von KEYWORD-ERROR
          pushSTACK(key);
          pushSTACK(STACK_(0+2));
          pushSTACK(Car(Cdr(arglistr)));
          pushSTACK(key);
          { var object type = allocate_cons();
            Car(type) = S(member); Cdr(type) = STACK_(0+5);
            STACK_3 = type; # `(MEMBER ,@kwlist) = Wert für Slot EXPECTED-TYPE von KEYWORD-ERROR
          }
          fehler(keyword_error,
                 GETTEXT("illegal keyword/value pair ~, ~ in argument list. The allowed keywords are ~")
                );
          found: # gefunden. Weiter:
          arglistr = Cdr(Cdr(arglistr));
    }   }
    fertig:
    skipSTACK(2);
    value1 = NIL; mv_count=0; # keine Werte
  }

LISPSPECFORM(and, 0,0,body)
# (AND {form}), CLTL S. 82
  { var object body = popSTACK();
    if (atomp(body))
      { value1 = T; mv_count=1; } # (AND) -> T
      else
      loop
        { pushSTACK(Cdr(body));
          eval(Car(body)); # form auswerten
          body = popSTACK();
          if (atomp(body)) break; # am Schluss: Werte der letzten Form zurück
          if (nullp(value1)) { mv_count=1; break; } # vorzeitig: 1 Wert NIL
        }
  }

LISPSPECFORM(or, 0,0,body)
# (OR {form}), CLTL S. 83
  { var object body = popSTACK();
    if (atomp(body))
      { value1 = NIL; mv_count=1; } # (OR) -> NIL
      else
      loop
        { pushSTACK(Cdr(body));
          eval(Car(body)); # form auswerten
          body = popSTACK();
          if (atomp(body)) break; # am Schluss: Werte der letzten Form zurück
          if (!nullp(value1)) { mv_count=1; break; } # vorzeitig: 1 Wert /=NIL
        }
  }

# Ab jetzt hat der Tabellenmacro eine andere Verwendung:
  #undef LISPSPECFORM

# Tabelle aller Fsubr-Funktionen:
  global const struct fsubr_tab_ fsubr_tab =
    {
      #define LISPSPECFORM LISPSPECFORM_D
      #include "fsubr.c"
      #undef LISPSPECFORM
    };

