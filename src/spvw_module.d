# Module management.

# ------------------------------ Specification ---------------------------------

# An executable can be built up of any number of modules. The main and
# first module is called "clisp". All modules have a specific subr_tab
# and a specific object_tab. The objects in these tables are saved in
# the mem file.
# There's a subtle bootstrapping involved with these tables:
# Let S denote a module set and M a new module.
# 1. Start with lisp.run (S) and lispinit.mem (S). If M needs some specific
#    packages to be defined, define them and re-save lispinit.mem (S).
# 2. Build lisp.run (S+M). Run lisp.run (S+M) with lispinit.mem (S).
#    The tables of S will be initialized from the mem file, whereas the
#    tables of M will be initialized freshly. Save as lispinit.mem (S+M).
# 3. Now run lisp.run (S+M) with lispinit.mem (S+M).
#    The tables of S+M will be initialized from the mem file.

# Initialize the module system.
  local void init_modules_0 (void);

# The main module, called "clisp".
  local module_ * main_module;

# Iterate through the modules.
# for_modules(which,statement);
# which = either `all_modules'
#         or     `all_other_modules' (means all except main_module).
# A variable `module' of type module_* must be declared; it will be used
# as the iteration variable and can use within statement.

#ifdef DYNAMIC_MODULES
# Register a new module.
  global void add_module (module_ * new_module);
#endif

# ------------------------------ Implementation --------------------------------

# These are defined in modules.d (which is re-compiled for every linking set).
  extern uintC module_count;
  extern module_ modules[];

#define main_module  modules[0]

#ifdef DYNAMIC_MODULES

  # A pointer to the last element of the module list.
  local module_ ** last_module;

  local void init_modules_0()
    { var module_* module = &modules[0];
      until (module[1].name==NULL) { module->next = module+1; module++; }
      *(last_module = &module->next) = NULL;
    }

  global void add_module(module)
    var module_ * module;
    { *last_module = module; last_module = &module->next;
      module_count++;
    }

  #define for_modules(which,statement)  \
    module = (which); until (module==NULL) { statement; module = module->next; }
  #define all_modules  &main_module
  #define all_other_modules  main_module.next

#else

  #define for_modules(which,statement)  \
    module = (which); until (module->name==NULL) { statement; module++; }
  #define all_modules  &modules[0]
  #define all_other_modules  &modules[1]

  #define init_modules_0()

#endif
