# Setting mark bits in objects.

# ------------------------------ Specification ---------------------------------

# To mark a heap object (for the purpose of GC or of circularity analysis), we
# set the garcol_bit in the first word (i.e. the GCself header for varobjects).
# Immediate objects can not (and need not) be marked.

# Set the mark bit at a given address.
# mark(addr);
# local void mark (void* addr);

# Clear the mark bit at a given address.
# unmark(addr);
# local void unmark (void* addr);

# Tests the mark bit at a given address.
# marked(addr)
# local boolean marked (void* addr);

# Add a mark bit to an object pointer.
# with_mark_bit(obj)
# local object with_mark_bit (object obj);

# Remove a mark bit from an object pointer.
# without_mark_bit(obj)
# local object without_mark_bit (object obj);

# ------------------------------ Implementation --------------------------------

  #define mark(addr)  (*(oint*)(addr) |= wbit(garcol_bit_o))

  #define unmark(addr)  (*(oint*)(addr) &= ~wbit(garcol_bit_o))

  #ifdef fast_mtypecode
    #define marked(addr)  (mtypecode(*(object*)(addr)) & bit(garcol_bit_t))
  #else
    #if !(garcol_bit_o == 32-1) || defined(WIDE)
      #define marked(addr)  (*(oint*)(addr) & wbit(garcol_bit_o))
    #else # garcol_bit_o = 32-1 = Vorzeichenbit
      #define marked(addr)  (*(sintL*)(addr) < 0)
    #endif
  #endif

  #define with_mark_bit(obj)  as_object(as_oint(obj) | wbit(garcol_bit_o))
  #define without_mark_bit(obj)  as_object(as_oint(obj) & ~wbit(garcol_bit_o))
