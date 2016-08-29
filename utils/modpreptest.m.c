#line 1 "/Users/sds/src/clisp/current/utils/modpreptest.c"
/* Sample module "FOO" */
// that's right!
// just a `modprep' test!

#define O(varname) module__sample__object_tab._##varname
#define F(varname) subr_tab_ptr_as_object(&(module__sample__subr_tab._##varname))

struct module__sample__object_tab_t {
#if ((defined(cond0)) && (defined(cond1))) || (cond2a ? cond2b : cond2c)
  gcv_object_t _object_Kgoto;
#endif
#if (defined(cond0)) && (!(defined(cond1)))
  gcv_object_t _object_Kotog;
#endif
  gcv_object_t _object_Ktest;
  gcv_object_t _object_Ktest_not;
#if (defined(cond0)) && (defined(cond1))
  gcv_object_t _object__23_28_29;
#endif
#if (!(cond2a ? cond2b : cond2c)) && (cond3)
  gcv_object_t _object__23_5Cspace;
#endif
#if (defined(cond0)) && (defined(cond1))
  gcv_object_t _object__28foo2_29;
#endif
#if (defined(cond0)) && (!(defined(cond1)))
  gcv_object_t _object__28foo3_29;
#endif
  gcv_object_t _var1;
#if ((defined(cond0)) && (defined(cond1))) || ((defined(cond0)) && (!(defined(cond1))))
  gcv_object_t _var2;
#endif
#if ((defined(cond0)) && (defined(cond1))) || ((defined(cond0)) && (!(defined(cond1))))
  gcv_object_t _var3;
#endif
} module__sample__object_tab;
uintC module__sample__object_tab_size = sizeof(module__sample__object_tab)/sizeof(gcv_object_t);

struct module__sample__object_tab_initdata_t {
#if ((defined(cond0)) && (defined(cond1))) || (cond2a ? cond2b : cond2c)
  object_initdata_t _object_Kgoto;
#endif
#if (defined(cond0)) && (!(defined(cond1)))
  object_initdata_t _object_Kotog;
#endif
  object_initdata_t _object_Ktest;
  object_initdata_t _object_Ktest_not;
#if (defined(cond0)) && (defined(cond1))
  object_initdata_t _object__23_28_29;
#endif
#if (!(cond2a ? cond2b : cond2c)) && (cond3)
  object_initdata_t _object__23_5Cspace;
#endif
#if (defined(cond0)) && (defined(cond1))
  object_initdata_t _object__28foo2_29;
#endif
#if (defined(cond0)) && (!(defined(cond1)))
  object_initdata_t _object__28foo3_29;
#endif
  object_initdata_t _var1;
#if ((defined(cond0)) && (defined(cond1))) || ((defined(cond0)) && (!(defined(cond1))))
  object_initdata_t _var2;
#endif
#if ((defined(cond0)) && (defined(cond1))) || ((defined(cond0)) && (!(defined(cond1))))
  object_initdata_t _var3;
#endif
  int _dummy_to_avoid_trailing_comma_in_initializer;
} module__sample__object_tab_initdata = {
#if ((defined(cond0)) && (defined(cond1))) || (cond2a ? cond2b : cond2c)
  { ":GOTO" },
#endif
#if (defined(cond0)) && (!(defined(cond1)))
  { ":OTOG" },
#endif
  { ":TEST" },
  { ":TEST-NOT" },
#if (defined(cond0)) && (defined(cond1))
  { "#()" },
#endif
#if (!(cond2a ? cond2b : cond2c)) && (cond3)
  { "#\\SPACE" },
#endif
#if (defined(cond0)) && (defined(cond1))
  { "(FOO2)" },
#endif
#if (defined(cond0)) && (!(defined(cond1)))
  { "(FOO3)" },
#endif
  { "NIL" },
#if ((defined(cond0)) && (defined(cond1))) || ((defined(cond0)) && (!(defined(cond1))))
  { "NIL" },
#endif
#if ((defined(cond0)) && (defined(cond1))) || ((defined(cond0)) && (!(defined(cond1))))
  { "NIL" },
#endif
  0
};

struct module__sample__subr_tab_t {
  VAROBJECTS_ALIGNMENT_DUMMY_DECL
#if defined(cond0)
  subr_t _subr_mypack_fun1;
#endif
#if ((defined(cond0)) && (defined(cond1))) || ((defined(cond0)) && (!(defined(cond1))))
  subr_t _subr_mypack_fun2;
#endif
  subr_t _subr_mypack_fun3;
  subr_t _subr_mypack_fun4;
  subr_t _subr_user_fun5;
  int _dummy_to_avoid_trailing_comma_in_initializer;
};
extern struct module__sample__subr_tab_t module__sample__subr_tab;



#line 4
#line 4
DEFMODULE(sample,"USER")
#line 10
#line 10
 DEFVAR (var1)
#ifdef cond0
 DEFUN (mypack:fun1, x,(subr_mypack_fun1,seclass_default,1,0,norest,nokey,0,NIL))
#if defined(cond1)
   push( O(object_Kgoto));
 DEFUN (mypack:fun2, x,(subr_mypack_fun2,seclass_default,1,0,norest,nokey,0,NIL))
   push(O(object__23_28_29));
   push( O(object_Kgoto));
 DEFVAR (var2)
 DEFVAR (var3)
#else
 DEFUN (mypack:fun2, y,(subr_mypack_fun2,seclass_default,1,0,norest,nokey,0,NIL))
   push(O(object_Kotog));
 DEFVAR (var3)
 DEFVAR (var2)
#endif
/* some test
   // `nested' *
   comments */
// with /* nesting
// at different " levels */
#endif
DEFUN (mypack:fun3, x y &optional z,(subr_mypack_fun3,seclass_default,2,1,norest,nokey,0,NIL))
DEFUN (mypack:fun4, x y &key test test-not,(subr_mypack_fun4,seclass_default,2,0,norest,key,2,NIL))
DEFUN (fun5, x y &rest r,(subr_user_fun5,seclass_default,2,0,rest,nokey,0,NIL))
{
#if cond2a ? cond2b : cond2c
   push(O(object_Kgoto));
#elif cond3
   push(O(object__23_5Cspace));
#endif
}


struct module__sample__subr_tab_t module__sample__subr_tab
  #if defined(HEAPCODES) && (alignment_long < varobject_alignment) && defined(__GNUC__)
    __attribute__ ((aligned (varobject_alignment)))
  #endif
  = {
  #if varobjects_misaligned
  { 0 },
  #endif
#if defined(cond0)
  LISPFUN_F(subr_mypack_fun1,seclass_default,1,0,norest,nokey,0,NIL)
#endif
#if ((defined(cond0)) && (defined(cond1))) || ((defined(cond0)) && (!(defined(cond1))))
  LISPFUN_F(subr_mypack_fun2,seclass_default,1,0,norest,nokey,0,NIL)
#endif
  LISPFUN_F(subr_mypack_fun3,seclass_default,2,1,norest,nokey,0,NIL)
  LISPFUN_F(subr_mypack_fun4,seclass_default,2,0,norest,key,2,NIL)
  LISPFUN_F(subr_user_fun5,seclass_default,2,0,rest,nokey,0,NIL)
  0
};
uintC module__sample__subr_tab_size = (sizeof(struct module__sample__subr_tab_t)-varobjects_misaligned-sizeof(int))/sizeof(subr_t);

struct module__sample__subr_tab_initdata_t {
#if defined(cond0)
  subr_initdata_t _subr_mypack_fun1;
#endif
#if ((defined(cond0)) && (defined(cond1))) || ((defined(cond0)) && (!(defined(cond1))))
  subr_initdata_t _subr_mypack_fun2;
#endif
  subr_initdata_t _subr_mypack_fun3;
  subr_initdata_t _subr_mypack_fun4;
  subr_initdata_t _subr_user_fun5;
  int _dummy_to_avoid_trailing_comma_in_initializer;
} module__sample__subr_tab_initdata = {
#if defined(cond0)
  { "mypack", "fun1" },
#endif
#if ((defined(cond0)) && (defined(cond1))) || ((defined(cond0)) && (!(defined(cond1))))
  { "mypack", "fun2" },
#endif
  { "mypack", "fun3" },
  { "mypack", "fun4" },
  { "USER", "fun5" },
  0
};

void module__sample__init_function_1 (module_t* module);
void module__sample__init_function_1 (module_t* module)
{
  pushSTACK(O(object_Ktest));
  pushSTACK(O(object_Ktest_not));
  module__sample__subr_tab._subr_mypack_fun4.keywords = vectorof(2);
  O(var1) = (NIL);
#if (defined(cond0)) && (defined(cond1))
  O(var2) = (O(object__28foo2_29));
#endif
#if (defined(cond0)) && (defined(cond1))
  O(var3) = (O(var2));
#endif
#if (defined(cond0)) && (!(defined(cond1)))
  O(var3) = (O(object__28foo3_29));
#endif
#if (defined(cond0)) && (!(defined(cond1)))
  O(var2) = (O(var3));
#endif
}

void module__sample__init_function_2 (module_t* module);
void module__sample__init_function_2 (module_t* module)
{
}

void module__sample__fini_function (module_t* module);
void module__sample__fini_function (module_t* module)
{
}
