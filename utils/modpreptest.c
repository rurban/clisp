/* Sample module "FOO" */
// that's right!
// just a `modprep' test!
#line 4
DEFMODULE(sample,"USER")
#line 10
/* comments can contain URLs: http://www.memri.org/ */
 DEFVAR (var1, NIL)
#ifdef cond0
 DEFUN (mypack:fun1, x)
#if defined(cond1)
   push( `:GOTO`);
 DEFUN (mypack:fun2, x)
   push(`#()`);
   push( `:GOTO`);
 DEFVAR (var2, `(foo2)`)
 DEFVAR (var3, O(var2))
#else
 DEFUN (mypack:fun2, y)
   push(`:OTOG`);
 DEFVAR (var3, `(foo3)`)
 DEFVAR (var2, O(var3))
#endif
/* some test
   // `nested' *
   comments */
// with /* nesting
// at different " levels */
#endif
DEFUN (mypack:fun3, x y &optional z)
DEFUN (mypack:fun4, x y &key test test-not)
DEFUN (fun5, x y &rest r)
{
#if cond2a ? cond2b : cond2c
   push(`:GOTO`);
#elif cond3
   push(`#\space`);
#endif
}
