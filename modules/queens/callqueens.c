/*
 * Interface to the queens() function
 */

#include "clisp.h"

#define nmax 100

/* extern declaration */
extern unsigned int queens (unsigned int n);

/* (USER::QUEENS n) returns the number of solutions to the n-queens problem.
 * n ought to be an integer > 0, <= nmax. Otherwise it returns NIL.
 */
LISPFUNN(queens,1)
{ /*
   * No garbage collection is a problem. So we get the argument from the
   * STACK immediately.
   */
  var object arg = popSTACK(); /* clean up STACK at the same time */
  /*
   * If arg is an integer > 0, <= 100, ist must be a nonnegative fixnum.
   * We do the argument check in two steps: 1. check whether arg is a
   * nonnegative fixnum. 2. Extract its value. 3. Check its value.
   */
  if (!posfixnump(arg)) goto bad_arg;
 {var uintL n = posfixnum_to_L(arg);
  if (!(n>0 && n<=nmax)) goto bad_arg;

  /* Arguments are checked. Do our job: */
  { var uint32 result;
    begin_call();
    result = queens(n); /* call external function */
    end_call();
    /*
     * Assume result is >=0 and <2^32 (which is guaranteed by the type
     * of problem we have and the amount of time queens() may have run).
     * So an `uint32' is enough, and the following call is appropriate.
     */
    value1 = uint32_to_I(result); /* convert result to nonnegative integer */
    mv_count=1; /* no "multiple" values */
  }
  return;
 }

  bad_arg:
    /* We could issue an error. We prefer to return NIL here. */
    value1 = NIL; mv_count=1; return;
}


/* linkable module for CLISP */

#undef LISPFUN
#define LISPFUN LISPFUN_F
#undef LISPSYM
#define LISPSYM(name,printname,package)  { package, printname },

subr_ module__queens__subr_tab [1] = {
  LISPFUNN(queens,1)
};

uintC module__queens__subr_tab_size = 1;

subr_initdata module__queens__subr_tab_initdata[1] = {
  LISPSYM(queens,"QUEENS","USER")
};

object module__queens__object_tab [1];

uintC module__queens__object_tab_size = 0;

object_initdata module__queens__object_tab_initdata[1];

void module__queens__init_function_1(module)
  var module_* module;
  { }

void module__queens__init_function_2(module)
  var module_* module;
  { }

