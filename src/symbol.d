# Funktionen betr. Symbole für CLISP
# Bruno Haible 1990-2002

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

/* Error when the symbol's property list has odd length.
 fehler_sym_plist_odd(symbol);
 > symbol: Symbol */
nonreturning_function(local, fehler_sym_plist_odd, (object symbol)) {
  pushSTACK(symbol);
  pushSTACK(S(get));
  fehler(error,GETTEXT("~: the property list of ~ has an odd length"));
}
/* Error when the property list has odd length
 fehler_plist_odd(caller,plist);
 > caller: Subr
 > plist: bad plist */
nonreturning_function(local, fehler_plist_odd, (object caller, object plist)) {
  pushSTACK(plist); pushSTACK(caller);
  fehler(error,GETTEXT("~: the property list ~ has an odd length"));
}

/* UP: find the key in the property list
 > plist_: the address of the plist
 > key: indicator
 < tail: eq(Car(*tail),key), or a pointer to an atom if not found,
         or NULL if odd length */
local inline gcv_object_t* plist_find (gcv_object_t *plist_, object key) {
  loop {
    var object plistr = *plist_;
    if (atomp(plistr)) /* not found */
      return plist_;
    if (eq(Car(plistr),key)) /* found */
      return plist_;
    plistr = Cdr(plistr);
    if (atomp(plistr)) /* odd length --> error */
      return NULL;
    plist_ = &Cdr(plistr);
  }
}

/* UP: remove the first value of key in plist
 > plist_: the address of the plist
 > key: indicator
 < 0: if odd length (error); 1: found; -1: not found */
local inline int plist_rem (gcv_object_t *plist_, object key) {
  var gcv_object_t *tail = plist_find(plist_,key);
  if (tail == NULL) return 0; /* odd length --> error */
  var object plistr = *tail;
  if (atomp(plistr)) return -1; /* key not found */
  plistr = Cdr(plistr);
  if (atomp(plistr)) return 0; /* odd length --> error */
  *tail = Cdr(plistr); /* shorten the property list by 2 elements */
  return 1;
}

/* UP: find a property in the property list of the symbol.
 get(symbol,key)
 > symbol: a Symbol
 > key: indicator
 < value: the value of key in the property list or unbound. */
global object get (object symbol, object key) {
  var gcv_object_t* plistr_ = plist_find(&(Symbol_plist(symbol)),key);
  if (plistr_ == NULL) /* property list has odd length */
    fehler_sym_plist_odd(symbol);
  var object plistr = *plistr_;
  if (atomp(plistr)) /* not found */
    return unbound;
  /* key found */
  plistr = Cdr(plistr);
  if (atomp(plistr))
    fehler_sym_plist_odd(symbol);
  return Car(plistr);
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
      fehler_lambda_expression(S(putd),fun);
    }
    fehler_function(fun);
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
{ /* (GETF place key [not-found]), CLTL p. 166 */
  var gcv_object_t *plistr_ = plist_find(&STACK_2,STACK_1);
  if (plistr_ == NULL) /* property list has odd length */
    fehler_plist_odd(S(getf),STACK_2);
  var object plistr = *plistr_;
  if (atomp(plistr)) { /* key not found */
    if (eq( value1 = STACK_0, unbound)) /* default value is not-found */
      value1 = NIL; /* if not supplied, then NIL. */
    mv_count=1; skipSTACK(3); return;
  }
  /* found key */
  plistr = Cdr(plistr);
  if (atomp(plistr))
    fehler_plist_odd(S(getf),STACK_2);
  VALUES1(Car(plistr)); skipSTACK(3);
}

LISPFUNN(putf,3)
{ /* (setf place (SYS::%PUTF place key value)) ==
     (setf (getf place key) value)
  see places.lisp: this will return NIL if no allocation was done, i.e.,
  if the list was modified "in place" and the PLACE does not have to be set */
  var gcv_object_t *tail = plist_find(&STACK_2,STACK_1);
  if (tail == NULL) fehler_plist_odd(S(putf),STACK_2);
  var object plistr = *tail;
  if (atomp(plistr)) { /* key not found => extend plist with 2 conses */
    pushSTACK(allocate_cons());
    var object cons1 = allocate_cons();
    var object cons2 = popSTACK();
    Car(cons2) = STACK_0; /* value */
    Cdr(cons2) = STACK_2; /* tail */
    Car(cons1) = STACK_1; /* key */
    Cdr(cons1) = cons2;
    VALUES1(cons1);
  } else {
    plistr = Cdr(plistr);
    if (atomp(plistr)) fehler_plist_odd(S(putf),STACK_2);
    Car(plistr) = STACK_0; /* value */
    VALUES1(NIL);
  }
  skipSTACK(3);
}

LISPFUNN(remf,2)
{ /* (remf place key) ==
     (multiple-value-bind (new-place removed-p) (SYS::%REMF place key)
       (when (and removed (null new-place)) (setf place new-place)) removed-p)
  see places.lisp: PLACE has to be modified only if the new value is ATOM */
  var gcv_object_t *tail = plist_find(&STACK_1,STACK_0);
  if (tail == NULL) fehler_plist_odd(S(remf),STACK_1);
  var object plistr = *tail;
  if (atomp(plistr)) value2 = NIL; /* key not found => not removed */
  else {
    plistr = Cdr(plistr);
    if (atomp(plistr)) fehler_plist_odd(S(remf),STACK_1);
    plistr = Cdr(plistr);
    if (atomp(plistr)) *tail = plistr;
    else { /* shorten the property list by 2 elements */
      Car(*tail) = Car(plistr);
      Cdr(*tail) = Cdr(plistr);
    }
    value2 = T;
  }
  value1 = STACK_1; mv_count = 2; skipSTACK(2);
}

LISPFUNN(get_properties,2)
{ /* (GET-PROPERTIES place keylist), CLTL p. 167 */
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
 found: /* key found */
  value3 = plistr; /* 3rd value = list rest */
  value1 = Car(plistr); /* 1st value = found key */
  plistr = Cdr(plistr);
  if (atomp(plistr))
    goto odd;
  value2 = Car(plistr); /* 2nd value = value for key */
  mv_count=3; return; /* 2 values */
 odd: /* property list has odd length */
  fehler_plist_odd(S(get_properties),plist);
 notfound: /* key not found */
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
{ /* (SYS::%PUT symbol key value) == (SETF (GET symbol key) value) */
  var object symbol = test_symbol(STACK_2);
  var gcv_object_t *tail = plist_find(&Symbol_plist(symbol),STACK_1);
  if (tail == NULL) /* property list has odd length */
    fehler_sym_plist_odd(symbol);
  var object plistr = *tail;
  if (atomp(plistr)) { /* key not found => extend plist with 2 conses */
    pushSTACK(allocate_cons());
    var object cons1 = allocate_cons();
    var object cons2 = popSTACK();
    Car(cons2) = STACK_0; /* value */
    Cdr(cons2) = Symbol_plist(STACK_2);
    Car(cons1) = STACK_1; /* key */
    Cdr(cons1) = cons2;
    Symbol_plist(STACK_2) = cons1;
  } else {
    plistr = Cdr(plistr);
    if (atomp(plistr)) fehler_sym_plist_odd(symbol); /* odd length --> error */
    Car(plistr) = STACK_0;
  }
  VALUES1(STACK_0);
  skipSTACK(3);
}

LISPFUNN(remprop,2)
{ /* (REMPROP symbol indicator), CLTL p. 166 */
  var object key = popSTACK();
  var object symbol = test_symbol(popSTACK());
  var gcv_object_t *tail = plist_find(&Symbol_plist(symbol),key);
  if (tail == NULL) fehler_sym_plist_odd(symbol);
  var object plistr = *tail;
  if (atomp(plistr)) value1 = NIL; /* key not found */
  else { /* key found */
    plistr = Cdr(plistr);
    if (atomp(plistr)) fehler_sym_plist_odd(symbol);
    *tail = Cdr(plistr); /* shorten the property list by 2 elements */
    value1 = T;
  }
  mv_count = 1;
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

