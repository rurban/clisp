# Liste aller zusätzlichen dem C-Programm bekannten Packages
# Bruno Haible 1990-1999

# Der Macro LISPPACK deklariert eine LISP-Package.
# LISPPACK(abbrev,packname)
# > abbrev: Kürzel, mit dem in constsym.d auf diese Package verwiesen wird
# > packname: C-Name der Package

# Expander für die Aufzählung:
  #define LISPPACK_A(abbrev,packname)  \
    enum_##abbrev##_index,

# Expander für die Konstruktion der Liste O(all_packages):
  #define LISPPACK_B(abbrev,packname)  \
    make_package(ascii_to_string(packname),NIL,FALSE);

# Welcher Expander benutzt wird, muss vom Hauptfile aus eingestellt werden.


LISPPACK(clos,"CLOS")
LISPPACK(user,"USER")
#ifdef SCREEN
LISPPACK(screen,"SCREEN")
#endif
#ifdef DYNAMIC_FFI
LISPPACK(ffi,"FFI")
#endif

