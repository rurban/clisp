# Funktionen betr. Symbole für CLISP
# Bruno Haible 1990-2001

#include "lispbibl.c"


#if 0 # unbenutzt
# UP: Liefert die globale Funktionsdefinition eines Symbols,
# mit Test, ob das Symbol eine globale Funktion darstellt.
# Symbol_function_checked(symbol)
# > symbol: Symbol
# < ergebnis: seine globale Funktionsdefinition
  global object Symbol_function_checked (object symbol);
  global object Symbol_function_checked(symbol)
    var object symbol;
    {
      var object fun = Symbol_function(symbol);
      if (! boundp(fun))
        fehler_undef_function(S(symbol_function),symbol);
      if (consp(fun)) {
        pushSTACK(symbol); # Wert für Slot NAME von CELL-ERROR
        pushSTACK(symbol);
        pushSTACK(S(function));
        fehler(undefined_function,
               GETTEXT("~: ~ is a macro, not a function"));
      }
      return fun;
    }
#endif

# Fehlermeldung, wenn ein Symbol eine Property-Liste ungerader Länge hat.
# fehler_plist_odd(symbol);
# > symbol: Symbol
nonreturning_function(local, fehler_plist_odd, (object symbol)) {
  pushSTACK(symbol);
  pushSTACK(S(get));
  fehler(error,GETTEXT("~: the property list of ~ has an odd length"));
}

# UP: Holt eine Property aus der Property-Liste eines Symbols.
# get(symbol,key)
# > symbol: ein Symbol
# > key: ein mit EQ zu vergleichender Key
# < value: dazugehöriger Wert aus der Property-Liste von symbol, oder unbound.
  global object get (object symbol, object key);
  global object get(symbol,key)
    var object symbol;
    var object key;
    {
      var object plistr = Symbol_plist(symbol);
      loop {
        if (atomp(plistr))
          goto notfound;
        if (eq(Car(plistr),key))
          goto found;
        plistr = Cdr(plistr);
        if (atomp(plistr))
          goto odd;
        plistr = Cdr(plistr);
      }
     found: # key gefunden
      plistr = Cdr(plistr);
      if (atomp(plistr))
        goto odd;
      return Car(plistr);
     odd: # Property-Liste hat ungerade Länge
      fehler_plist_odd(symbol);
     notfound: # key nicht gefunden
      return unbound;
    }

/* UP: check whether the argument is a symbol and return it */
local inline object test_symbol (object sy) {
  if (!symbolp(sy)) fehler_symbol(sy);
  return sy;
}

LISPFUNN(putd,2)
# (SYS::%PUTD symbol function)
  {
    var object symbol = test_symbol(STACK_1);
    var object fun = STACK_0;
    # fun muss SUBR, FSUBR, Closure oder #<MACRO expander> sein,
    # Lambda-Ausdruck ist nicht mehr gültig.
    if (functionp(fun) || fsubrp(fun))
      goto ok;
    elif (macrop(fun)) # #<MACRO expander> ist ok
      goto ok;
    elif (consp(fun) && eq(Car(fun),S(lambda))) { # eine Lambda-Expression?
      fehler_lambda_expression(fun);
    }
    pushSTACK(fun); # TYPE-ERROR slot DATUM
    pushSTACK(S(function)); # TYPE-ERROR slot EXPECTED-TYPE
    pushSTACK(fun);
    fehler(type_error,
           GETTEXT("SETF SYMBOL-FUNCTION: ~ is not a function")
          );
   ok: # fun korrekt, in die Funktionszelle stecken:
    VALUES1(popSTACK()); /* return function-Argument */
    Symbol_function(popSTACK()) = fun;
  }

LISPFUNN(find_subr,1)
# (SYS::%FIND-SUBR symbol)
# (defun sys::%find-subr (symbol)
#   (assert (symbolp symbol))
#   (or (get symbol 'sys::traced-definition) (symbol-function symbol))
# )
  {
    var object symbol = test_symbol(popSTACK());
    var object result = get(symbol,S(traced_definition));
    if (! boundp(result))
      result = Symbol_function(symbol);
    if (!subrp(result)) {
      pushSTACK(symbol);
      pushSTACK(S(find_subr));
      fehler(error,GETTEXT("~: ~ is not a system function"));
    }
    VALUES1(result);
  }

LISPFUNN(proclaim_constant,2)
# (SYS::%PROCLAIM-CONSTANT symbol value) erklärt ein Symbol zu einer Konstanten
# und ihm einen Wert zu.
  {
    var object val = popSTACK();
    var object symbol = test_symbol(popSTACK());
    set_const_flag(TheSymbol(symbol)); # symbol zu einer Konstanten machen
    Symbol_value(symbol) = val; # ihren Wert setzen
    VALUES1(symbol); /* return symbol */
  }

LISPFUN(get,2,1,norest,nokey,0,NIL)
# (GET symbol key [not-found]), CLTL S. 164
  {
    var object symbol = test_symbol(STACK_2);
    var object result = get(symbol,STACK_1); # suchen
    if (! boundp(result)) { /* not found? */
      result = STACK_0; # Defaultwert ist not-found
      if (! boundp(result)) /* not supplied */
        result = NIL; # dann NIL.
    }
    VALUES1(result);
    skipSTACK(3);
  }

LISPFUN(getf,2,1,norest,nokey,0,NIL)
# (GETF place key [not-found]), CLTL S. 166
  {
    var object plistr = STACK_2;
    var object key = STACK_1;
    loop {
      if (atomp(plistr))
        goto notfound;
      if (eq(Car(plistr),key))
        goto found;
      plistr = Cdr(plistr);
      if (atomp(plistr))
        goto odd;
      plistr = Cdr(plistr);
    }
   found: # key gefunden
    plistr = Cdr(plistr);
    if (atomp(plistr))
      goto odd;
    VALUES1(Car(plistr)); skipSTACK(3); return;
   odd: # Property-Liste hat ungerade Länge
    pushSTACK(STACK_2);
    pushSTACK(S(getf));
    fehler(error,
           GETTEXT("~: the property list ~ has an odd length")
          );
   notfound: # key nicht gefunden
    if (eq( value1 = STACK_0, unbound)) # Defaultwert ist not-found
      value1 = NIL; # Ist der nicht angegeben, dann NIL.
    mv_count=1; skipSTACK(3); return;
  }

LISPFUNN(get_properties,2)
# (GET-PROPERTIES place keylist), CLTL S. 167
  {
    var object keylist = popSTACK();
    var object plist = popSTACK();
    var object plistr = plist;
    loop {
      if (atomp(plistr))
        goto notfound;
      var object item = Car(plistr);
      if (!nullp(memq(item,keylist)))
        goto found;
      plistr = Cdr(plistr);
      if (atomp(plistr))
        goto odd;
      plistr = Cdr(plistr);
    }
   found: # key gefunden
    value3 = plistr; # Dritter Wert = Listenrest
    value1 = Car(plistr); # Erster Wert = gefundener Key
    plistr = Cdr(plistr);
    if (atomp(plistr))
      goto odd;
    value2 = Car(plistr); # Zweiter Wert = Wert zum Key
    mv_count=3; return; # Drei Werte
   odd: # Property-Liste hat ungerade Länge
    pushSTACK(plist);
    pushSTACK(S(get_properties));
    fehler(error,
           GETTEXT("~: the property list ~ has an odd length")
          );
   notfound: # key nicht gefunden
    VALUES3(NIL,NIL,NIL); return; /* all 3 values */
  }

LISPFUNN(putplist,2)
# (SYS::%PUTPLIST symbol list) == (SETF (SYMBOL-PLIST symbol) list)
  {
    var object list = popSTACK();
    var object symbol = test_symbol(popSTACK());
    VALUES1(Symbol_plist(symbol) = list);
  }

LISPFUNN(put,3)
# (SYS::%PUT symbol key value) == (SETF (GET symbol key) value)
  {
    {
      var object symbol = test_symbol(STACK_2);
      var object key = STACK_1;
      var object plistr = Symbol_plist(symbol);
      loop {
        if (atomp(plistr))
          goto notfound;
        if (eq(Car(plistr),key))
          goto found;
        plistr = Cdr(plistr);
        if (atomp(plistr))
          goto odd;
        plistr = Cdr(plistr);
      }
     found: # key gefunden
      plistr = Cdr(plistr);
      if (atomp(plistr))
        goto odd;
      VALUES1(Car(plistr) = STACK_0); /* fill new value */
      skipSTACK(3); return;
     odd: # Property-Liste hat ungerade Länge
      fehler_plist_odd(symbol);
    }
   notfound: # key nicht gefunden
    {
      # Property-Liste um 2 Conses erweitern:
      pushSTACK(allocate_cons());
      var object cons1 = allocate_cons();
      var object cons2 = popSTACK();
      VALUES1(Car(cons2) = popSTACK()); /* value */
      Car(cons1) = popSTACK(); # key
      var object symbol = popSTACK();
      Cdr(cons2) = Symbol_plist(symbol);
      Cdr(cons1) = cons2;
      Symbol_plist(symbol) = cons1;
      return;
    }
  }

LISPFUNN(remprop,2)
# (REMPROP symbol indicator), CLTL S. 166
  {
    var object key = popSTACK();
    var object symbol = test_symbol(popSTACK());
    var object* plistr_ = &Symbol_plist(symbol);
    var object plistr;
    loop {
      plistr = *plistr_;
      if (atomp(plistr))
        goto notfound;
      if (eq(Car(plistr),key))
        goto found;
      plistr = Cdr(plistr);
      if (atomp(plistr))
        goto odd;
      plistr_ = &Cdr(plistr);
    }
   found: # key gefunden
    plistr = Cdr(plistr);
    if (atomp(plistr))
      goto odd;
    *plistr_ = Cdr(plistr); # Property-Liste um 2 Elemente verkürzen
    VALUES1(T); return;
   odd: # Property-Liste hat ungerade Länge
    fehler_plist_odd(symbol);
   notfound: # key nicht gefunden
    VALUES1(NIL); return;
  }

LISPFUNN(symbol_package,1)
# (SYMBOL-PACKAGE symbol), CLTL S. 170
  {
    var object symbol = test_symbol(popSTACK());
    VALUES1(Symbol_package(symbol));
  }

LISPFUNN(symbol_plist,1)
# (SYMBOL-PLIST symbol), CLTL S. 166
  {
    var object symbol = test_symbol(popSTACK());
    VALUES1(Symbol_plist(symbol));
  }

LISPFUNN(symbol_name,1)
# (SYMBOL-NAME symbol), CLTL S. 168
  {
    var object symbol = test_symbol(popSTACK());
    VALUES1(Symbol_name(symbol));
  }

LISPFUNN(keywordp,1)
# (KEYWORDP object), CLTL S. 170
  {
    var object obj = popSTACK();
    VALUES_IF(symbolp(obj) && keywordp(obj));
  }

LISPFUNN(special_variable_p,1)
# (SYS::SPECIAL-VARIABLE-P symbol) stellt fest, ob das Symbol eine
# Special-Variable (oder eine Konstante) darstellt.
# (Bei Konstanten ist ja das Special-Bit bedeutungslos.)
  {
    var object symbol = test_symbol(popSTACK());
    VALUES_IF(constantp(TheSymbol(symbol)) || special_var_p(TheSymbol(symbol)));
  }

LISPFUN(gensym,0,1,norest,nokey,0,NIL)
# (GENSYM x), CLTL S. 169, CLtL2 S. 245-246
# (defun gensym (&optional (x nil s))
#   (let ((prefix "G") ; ein String
#         (counter *gensym-counter*)) ; ein Integer >=0
#     (when s
#       (cond ((stringp x) (setq prefix x))
#             ((integerp x)
#              (if (minusp x)
#                (error-of-type 'type-error
#                       :datum x :expected-type '(INTEGER 0 *)
#                       (ENGLISH "~S: index ~S is negative")
#                       'gensym x
#                )
#                (setq counter x)
#             ))
#             (t (error-of-type 'type-error
#                       :datum x :expected-type '(OR STRING INTEGER)
#                       (ENGLISH "~S: invalid argument ~S")
#                       'gensym x
#             )  )
#     ) )
#     (prog1
#       (make-symbol
#         (string-concat
#           prefix
#           #-CLISP (write-to-string counter :base 10 :radix nil)
#           #+CLISP (sys::decimal-string counter)
#       ) )
#       (unless (integerp x) (setq *gensym-counter* (1+ counter)))
# ) ) )
  {
    var object prefix = O(gensym_prefix); # "G"
    var object counter = Symbol_value(S(gensym_counter)); # *GENSYM-COUNTER*
    var object x = popSTACK(); # Argument
    if (boundp(x)) {
      # x angegeben
      if (stringp(x)) {
        prefix = x; # prefix setzen
      } elif (integerp(x)) {
        if (R_minusp(x)) {
          pushSTACK(x); # TYPE-ERROR slot DATUM
          pushSTACK(O(type_posinteger)); # TYPE-ERROR slot EXPECTED-TYPE
          pushSTACK(x);
          pushSTACK(S(gensym));
          fehler(type_error,
                 GETTEXT("~: index ~ is negative")
                );
        }
        # x ist ein Integer >=0
        counter = x; # counter setzen
      } else {
        pushSTACK(x); # TYPE-ERROR slot DATUM
        pushSTACK(O(type_string_integer)); # TYPE-ERROR slot EXPECTED-TYPE
        pushSTACK(x);
        pushSTACK(S(gensym));
        fehler(type_error,
               GETTEXT("~: invalid argument ~")
              );
      }
    }
    # String zusammenbauen:
    pushSTACK(prefix); # 1. Teilstring
    pushSTACK(counter); # counter
    if (!integerp(x)) {
      if (!(integerp(counter) && !R_minusp(counter))) { # sollte Integer >= 0 sein
        var object new_value = Symbol_value(S(gensym_counter)) = Fixnum_0; # *GENSYM-COUNTER* zurücksetzen
        pushSTACK(counter);            # TYPE-ERROR slot DATUM
        pushSTACK(O(type_posinteger)); # TYPE-ERROR slot EXPECTED-TYPE
        pushSTACK(new_value); pushSTACK(counter);
        fehler(type_error,
               GETTEXT("The value of *GENSYM-COUNTER* was not a nonnegative integer. Old value ~. New value ~.")
              );
      }
      Symbol_value(S(gensym_counter)) = I_1_plus_I(counter); # (incf *GENSYM-COUNTER*)
    }
    funcall(L(decimal_string),1); # (sys::decimal-string counter)
    pushSTACK(value1); # 2. String
    VALUES1(make_symbol(coerce_imm_ss(string_concat(2))));
  }

