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
# local bool marked (void* addr);

# Add a mark bit to an object pointer.
# with_mark_bit(obj)
# local object with_mark_bit (object obj);

# Remove a mark bit from an object pointer.
# without_mark_bit(obj)
# local object without_mark_bit (object obj);

# ------------------------------ Implementation --------------------------------

  #if defined(WIDE_STRUCT) || defined(OBJECT_STRUCT)
    #define mark(addr)  (((object*)(addr))->one |= wbit(garcol_bit_o))
  #else
    #define mark(addr)  (*(object*)(addr) = as_object(as_oint(*(object*)(addr)) | wbit(garcol_bit_o)))
  #endif

  #if defined(WIDE_STRUCT) || defined(OBJECT_STRUCT)
    #define unmark(addr)  (((object*)(addr))->one &= ~wbit(garcol_bit_o))
  #else
    #define unmark(addr)  (*(object*)(addr) = as_object(as_oint(*(object*)(addr)) & ~wbit(garcol_bit_o)))
  #endif

  #ifdef fast_mtypecode
    #define marked(addr)  (mtypecode(*(object*)(addr)) & bit(garcol_bit_t))
  #else
    #define marked(addr)  (as_oint(*(object*)(addr)) & wbit(garcol_bit_o))
  #endif

  #define with_mark_bit(obj)  as_object(as_oint(obj) | wbit(garcol_bit_o))
  #define without_mark_bit(obj)  as_object(as_oint(obj) & ~wbit(garcol_bit_o))
