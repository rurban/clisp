# An alloca() replacement, used for DYNAMIC_ARRAY.

# ------------------------------ Specification ---------------------------------

#ifdef NEED_MALLOCA

  # Allocates a block of memory of a given size. It can be freed (but needs
  # not to be freed) using freea(ptr).
    extern void* malloca (size_t size);

  # Frees all memory blocks allocated by malloca() since ptr was allocated,
  # including ptr itself.
    extern void freea (void* ptr);

#endif

# ------------------------------ Implementation --------------------------------

#ifdef NEED_MALLOCA

# Make sure malloc() and free() are declared.
#ifndef malloc
  extern_C void* malloc (size_t size); # siehe MALLOC(3V)
#endif
#ifndef free
  extern_C void free (void* ptr); # siehe MALLOC(3V)
#endif

# The allocated memory blocks are a linked list.
typedef struct malloca_header
               { struct malloca_header * next;
                 oint usable_memory[unspecified]; # "oint" forces alignment
               }
        malloca_header;

# Linked list of blocks, the most recent in front, the oldest at the end.
  local malloca_header* malloca_list = NULL;

  global void* malloca (size_t size);
  global void* malloca(size)
    var size_t size;
    { var malloca_header* ptr = (malloca_header*)malloc(offsetofa(malloca_header,usable_memory) + size);
      if (!(ptr == NULL))
        { ptr->next = malloca_list;
          malloca_list = ptr;
          return &ptr->usable_memory;
        }
        else
        {
          #ifdef VIRTUAL_MEMORY
          asciz_out(GETTEXT(NLstring "*** - " "Virtual memory exhausted. RESET"));
          #else
          asciz_out(GETTEXT(NLstring "*** - " "Memory exhausted. RESET")); :
          #endif
          reset();
    }   }

  global void freea (void* ptr);
  global void freea(address)
    var void* address;
    { var malloca_header* ptr = (malloca_header*)
        ((aint)address - offsetofa(malloca_header,usable_memory));
      var malloca_header* p = malloca_list;
      loop
        { var malloca_header* n = p->next;
          free(p);
          if (!(p == ptr))
            { p = n; }
            else
            { malloca_list = n; break; }
        }
    }

#endif
