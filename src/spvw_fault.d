# Page fault and protection handling. Support for SELFMADE_MMAP.

# ------------------------------ Specification ---------------------------------

# Physical page size. When a fault occurs, an entire physical page must
# change its protections.
  local /* uintL */ aint physpagesize;  # = map_pagesize or mmap_pagesize

# 2^physpageshift = physpagesize
  local uintL physpageshift;

# Initialization.
# local void init_physpagesize (void);

# Tries to repair a page fault at a single address.
  typedef enum { handler_failed, handler_done }
          handle_fault_result;
  local handle_fault_result handle_fault (aint address);

#if (defined(GENERATIONAL_GC) && defined(SPVW_MIXED)) || defined(SELFMADE_MMAP)
# Tries to repair a fault spanning a range of pages.
# handle_fault_range(PROT_READ,start,end) ensures an address range is readable.
# handle_fault_range(PROT_READ_WRITE,start,end) makes it writable.
  global boolean handle_fault_range (int prot, aint start_address, aint end_address);
#endif

#ifdef SELFMADE_MMAP

# Simulate an mmap for the given heap, of length map_len.
# map_len must be a positive multiple of physpagesize.
  local int selfmade_mmap (Heap* heap, uintL map_len, uintL offset);

#endif

#ifdef GENERATIONAL_GC

# Does the same as mprotect.
# Aborts if unsuccessful.
local void xmprotect (aint addr, uintL len, int prot);

# Applies mprotect to all (multi-)mappings of an address range.
# Aborts if unsuccessful.
local void xmmprotect (aint addr, uintL len, int prot);

#endif

# ------------------------------ Implementation --------------------------------

# Initialization.
  #define init_physpageshift()  \
    { var uintL x = physpagesize;                 \
      var uintL i = 0;                            \
      until ((x >>= 1) == 0) { i++; }             \
      if (!((1UL << i) == physpagesize)) abort(); \
      physpageshift = i;                          \
    }
  #ifdef MAP_MEMORY
    #define init_physpagesize()  \
      physpagesize = map_pagesize; \
      init_physpageshift();
  #else
    #define init_physpagesize()  \
      physpagesize = mmap_pagesize; \
      init_physpageshift();
  #endif

#ifdef SELFMADE_MMAP # impliziert SPVW_PURE_BLOCKS <==> SINGLEMAP_MEMORY
                     # oder       SPVW_MIXED_BLOCKS_STAGGERED

# Unterroutine f�rs Lesen einer Page vom mem-File.
local int handle_mmap_fault (uintL offset, aint address, uintB* memfile_page);
local int handle_mmap_fault(offset,address,memfile_page)
  var uintL offset;
  var aint address;
  var uintB* memfile_page;
  { if (*memfile_page == 0)
      # Page already in memory, nothing to be done.
      return 0;
    # Fetch the page from the file.
    { var Handle handle = mem.memfile_handle;
      var sintL orig_offset = 0;
      # If loadmem() is still reading from the memfile, we must be careful
      # to restore the handle's file position. (This could be avoided under
      # UNIX by using dup(), but not on WIN32_NATIVE.)
      if (mem.memfile_still_being_read)
        { orig_offset = lseek(handle,0,SEEK_CUR);
          if (orig_offset < 0)
            { asciz_out("selfmade_mmap: lseek() failed.");
              errno_out(OS_errno);
              return -1;
            }
        }
      if (zeromap((void*)address,physpagesize) < 0)
        { asciz_out("selfmade_mmap: zeromap() failed.");
          return -1;
        }
      if (lseek(handle,offset,SEEK_SET) < 0)
        { asciz_out_1("selfmade_mmap: lseek(0x%x) failed.",offset);
          errno_out(OS_errno);
          return -1;
        }
      #ifdef DEBUG_SPVW
      asciz_out_2("selfmade_mmap: address=0x%x <-- offset=0x%x" NLstring,address,offset);
      #endif
     {var sintL res;
      #ifdef WIN32_NATIVE
      # Call ReadFile(), not full_read(), because we don't want to handle Ctrl-C now.
      if (!ReadFile(handle,(void*)address,physpagesize,&res,NULL)) { res = -1; }
      #else
      res = full_read(handle,(void*)address,physpagesize);
      #endif
      if (!(res == physpagesize))
        { asciz_out_3("selfmade_mmap: full_read(offset=0x%x,count=%d) failed, returned %d.",offset,physpagesize,res);
          if (res < 0) errno_out(OS_errno);
          return -1;
        }
      if (mem.memfile_still_being_read)
        { if (lseek(handle,orig_offset,SEEK_SET) < 0)
            { asciz_out_1("selfmade_mmap: lseek(0x%x) failed.",orig_offset);
              errno_out(OS_errno);
              return -1;
            }
        }
      # Done.
      *memfile_page = 0;
      return 1;
  } }}

#endif # SELFMADE_MMAP

#ifdef GENERATIONAL_GC # impliziert SPVW_PURE_BLOCKS <==> SINGLEMAP_MEMORY
                       # oder       SPVW_MIXED_BLOCKS_STAGGERED
                       # oder       SPVW_MIXED_BLOCKS_OPPOSITE

# Unterroutine f�r protection: PROT_NONE -> PROT_READ
local int handle_read_fault (aint address, physpage_state* physpage);
local int handle_read_fault(address,physpage)
  var aint address;
  var physpage_state* physpage;
  { # Seite auf den Stand des Cache bringen:
    { var uintL count = physpage->cache_size;
      if (count > 0)
        { var old_new_pointer* ptr = physpage->cache;
          #if !defined(MULTIMAP_MEMORY)
          if (mprotect((MMAP_ADDR_T)address, physpagesize, PROT_READ_WRITE) < 0)
            return -1;
          #endif
          dotimespL(count,count, { *(ptr->p) = ptr->o; ptr++; } );
    }   }
    # Seite read-only einblenden:
    #if !defined(MULTIMAP_MEMORY)
    if (mprotect((MMAP_ADDR_T)address, physpagesize, PROT_READ) < 0)
      return -1;
    #else # MULTIMAP_MEMORY
    #if !defined(WIDE_SOFT)
    ASSERT(address == upointer(address));
    #endif
    { var uintL type;
      for (type = 0; type < typecount; type++)
        if (mem.heapnr_from_type[type] >= 0) # type in MM_TYPECASES aufgef�hrt?
          { if (mprotect((MMAP_ADDR_T)combine(type,address), physpagesize, PROT_READ) < 0)
              return -1;
    }     }
    #endif
    physpage->protection = PROT_READ;
    return 0;
  }

# Unterroutine f�r protection: PROT_READ -> PROT_READ_WRITE
local int handle_readwrite_fault (aint address, physpage_state* physpage);
local int handle_readwrite_fault(address,physpage)
  var aint address;
  var physpage_state* physpage;
  { # Seite read-write einblenden:
    #if !defined(MULTIMAP_MEMORY)
    if (mprotect((MMAP_ADDR_T)address, physpagesize, PROT_READ_WRITE) < 0)
      return -1;
    #else # MULTIMAP_MEMORY
    ASSERT(address == upointer(address));
    { var uintL type;
      for (type = 0; type < typecount; type++)
        if (mem.heapnr_from_type[type] >= 0) # type in MM_TYPECASES aufgef�hrt?
          if (mprotect((MMAP_ADDR_T)combine(type,address), physpagesize, PROT_READ_WRITE) < 0)
            return -1;
    }
    #endif
    physpage->protection = PROT_READ_WRITE;
    return 0;
  }

# mapped generation: the old one
#define heap_mgen_start  heap_gen0_start
#define heap_mgen_end    heap_gen0_end

#else

#define heap_mgen_start  heap_start
#define heap_mgen_end    heap_end

#endif # GENERATIONAL_GC

local handle_fault_result handle_fault(address)
  var aint address;
  { var uintL heapnr;
    var object obj = as_object((oint)address << oint_addr_shift);
    var aint uaddress = canon(address); # hoffentlich = canonaddr(obj);
    var aint pa_uaddress = uaddress & -physpagesize; # page aligned address
    #ifdef SPVW_PURE_BLOCKS
    heapnr = typecode(obj);
    #elif defined(SPVW_MIXED_BLOCKS_STAGGERED)
    heapnr = (uaddress >= mem.heaps[1].heap_mgen_start ? 1 : 0);
    #else # SPVW_MIXED_BLOCKS_OPPOSITE
    heapnr = (uaddress >= mem.heaps[1].heap_start ? 1 : 0);
    #endif
    {var Heap* heap = &mem.heaps[heapnr];
     var uintL pageno;
     #ifdef SELFMADE_MMAP
     if (is_unused_heap(heapnr)) goto error1;
     if (!((heap->heap_mgen_start <= uaddress) && (uaddress < heap->heap_mgen_end)))
       goto error2;
     pageno = (pa_uaddress>>physpageshift)-(heap->heap_mgen_start>>physpageshift);
     if (pageno < heap->memfile_numpages)
       { var int did_pagein =
           handle_mmap_fault(heap->memfile_offset+(pageno<<physpageshift),
                             pa_uaddress,
                             &heap->memfile_pages[pageno]);
         if (did_pagein < 0) goto error3;
         if (did_pagein)
           {
             #ifdef GENERATIONAL_GC
             if (heap->physpages == NULL) # implicitly PROT_READ_WRITE
               return handler_done;
             switch (heap->physpages[pageno].protection)
               { case PROT_READ_WRITE:
                   return handler_done;
                 #ifdef SPVW_PURE_BLOCKS
                 case PROT_READ:
                   if (mprotect((MMAP_ADDR_T)pa_uaddress,physpagesize,PROT_READ) < 0) goto error3;
                   return handler_done;
                 case PROT_NONE:
                   if (mprotect((MMAP_ADDR_T)pa_uaddress,physpagesize,PROT_NONE) < 0) goto error3;
                   return handler_done;
                 #endif
                 default:
                   goto error4;
               }
             #else
             return handler_done;
             #endif
           }
       }
     #endif
     #ifdef GENERATIONAL_GC
     if (!is_heap_containing_objects(heapnr)) goto error1;
     if (!((heap->heap_gen0_start <= uaddress) && (uaddress < heap->heap_gen0_end))) goto error2;
     if (heap->physpages == NULL) goto error5;
     #ifndef SELFMADE_MMAP
     pageno = (pa_uaddress>>physpageshift)-(heap->heap_gen0_start>>physpageshift);
     #endif
     {var physpage_state* physpage = &heap->physpages[pageno];
      switch (physpage->protection)
        { case PROT_NONE:
            # protection: PROT_NONE -> PROT_READ
            if (handle_read_fault(pa_uaddress,physpage) < 0) goto error6;
            return handler_done;
          case PROT_READ:
            # protection: PROT_READ -> PROT_READ_WRITE
            if (handle_readwrite_fault(pa_uaddress,physpage) < 0) goto error7;
            return handler_done;
          case PROT_READ_WRITE:
            goto error8;
          default:
            goto error9;
        }
      error6: # handle_read_fault() failed
        { var int saved_errno = OS_errno;
          asciz_out_2(NLstring "*** - " "handle_fault error6 ! mprotect(0x%x,0x%x,...) -> ", address & -physpagesize, physpagesize);
          errno_out(saved_errno);
        }
        goto error;
      error7: # handle_readwrite_fault() failed
        { var int saved_errno = OS_errno;
          asciz_out_3(NLstring "*** - " "handle_fault error7 ! mprotect(0x%x,0x%x,%d) -> ", address & -physpagesize, physpagesize, PROT_READ_WRITE);
          errno_out(saved_errno);
        }
        goto error;
      error8: # fault on a read-write page
        asciz_out_1(NLstring "*** - " "handle_fault error8 ! protection = %d", physpage->protection);
        goto error;
      error9: # invalid protection value
        asciz_out_1(NLstring "*** - " "handle_fault error9 ! protection = %d", physpage->protection);
        goto error;
     }
     error5: # fault on a read-write page with no physpages array
       asciz_out(NLstring "*** - " "handle_fault error5 !");
       goto error;
     #endif
     error1: # A fault was not expected on this type of heap.
       asciz_out(NLstring "*** - " "handle_fault error1 !");
       goto error;
     error2: # The address is outside of the used address range for this heap.
       asciz_out_3(NLstring "*** - " "handle_fault error2 ! address = 0x%x not in [0x%x,0x%x) !", address, heap->heap_mgen_start, heap->heap_mgen_end);
       goto error;
     #ifdef SELFMADE_MMAP
     error3: # handle_mmap_fault() failed
       asciz_out(NLstring "*** - " "handle_fault error3 !");
       goto error;
     #endif
     #if defined(SELFMADE_MMAP) && defined(GENERATIONAL_GC)
     error4: # The page ought not to be read-write, although we just paged it in.
       asciz_out_1(NLstring "*** - " "handle_fault error4 ! protection = %d",heap->physpages[pageno].protection);
       goto error;
     #endif
    }
    error:
    return handler_failed;
  }

#if (defined(GENERATIONAL_GC) && defined(SPVW_MIXED_BLOCKS)) || defined(SELFMADE_MMAP)
# Systemaufrufe wie read() und write() melden kein SIGSEGV, sondern EFAULT.
# handle_fault_range(PROT_READ,start,end) macht einen Adressbereich lesbar,
# handle_fault_range(PROT_READ_WRITE,start,end) macht ihn schreibbar.
global boolean handle_fault_range (int prot, aint start_address, aint end_address);
global boolean handle_fault_range(prot,start_address,end_address)
  var int prot;
  var aint start_address;
  var aint end_address;
  { start_address = canon(start_address);
    end_address = canon(end_address);
    if (!(start_address < end_address)) { return TRUE; }
   {var Heap* heap = &mem.heaps[0]; # varobject_heap
    var boolean did_pagein = FALSE;
    if ((end_address <= heap->heap_mgen_start) || (heap->heap_mgen_end <= start_address))
      return TRUE; # nichts zu tun, aber seltsam, dass �berhaupt ein Fehler kam
    #ifdef SELFMADE_MMAP
    if (heap->memfile_numpages > 0)
      { var aint pa_uaddress;
        for (pa_uaddress = start_address & -physpagesize; pa_uaddress < end_address; pa_uaddress += physpagesize)
          if ((heap->heap_mgen_start <= pa_uaddress) && (pa_uaddress < heap->heap_mgen_end))
            { var uintL pageno = (pa_uaddress>>physpageshift)-(heap->heap_mgen_start>>physpageshift);
              if (pageno < heap->memfile_numpages)
                switch (handle_mmap_fault(heap->memfile_offset+(pageno<<physpageshift),
                                          pa_uaddress,
                                          &heap->memfile_pages[pageno]))
                  { case 1:
                      did_pagein = TRUE;
                      #if defined(GENERATIONAL_GC) && defined(SPVW_PURE_BLOCKS)
                      if (!(heap->physpages == NULL))
                        switch (heap->physpages[pageno].protection)
                          { case PROT_NONE:
                              if (!(prot == PROT_READ || prot == PROT_READ_WRITE))
                                { if (mprotect((MMAP_ADDR_T)pa_uaddress,physpagesize,PROT_NONE) < 0)
                                    return FALSE;
                                  break;
                                }
                              if (handle_read_fault(pa_uaddress,&heap->physpages[pageno]) < 0)
                                return FALSE;
                              /* fallthrough */
                            case PROT_READ:
                              if (!(prot == PROT_READ_WRITE))
                                { if (mprotect((MMAP_ADDR_T)pa_uaddress,physpagesize,PROT_READ) < 0)
                                    return FALSE;
                                  break;
                                }
                              if (handle_readwrite_fault(pa_uaddress,&heap->physpages[pageno]) < 0)
                                return FALSE;
                              /* fallthrough */
                            case PROT_READ_WRITE:
                              break;
                          }
                      #endif
                      break;
                    case 0: break;
                    default: return FALSE;
            }     }
      }
    #endif
    #ifdef GENERATIONAL_GC
    if (heap->physpages == NULL)
      { if (did_pagein) { return TRUE; }
        return FALSE;
      }
    { var aint pa_uaddress;
      for (pa_uaddress = start_address & -physpagesize; pa_uaddress < end_address; pa_uaddress += physpagesize)
        if ((heap->heap_gen0_start <= pa_uaddress) && (pa_uaddress < heap->heap_gen0_end))
          { var uintL pageno = (pa_uaddress>>physpageshift)-(heap->heap_gen0_start>>physpageshift);
            var physpage_state* physpage = &heap->physpages[pageno];
            if ((physpage->protection == PROT_NONE) && (prot == PROT_READ || prot == PROT_READ_WRITE))
              # protection: PROT_NONE -> PROT_READ
              { if (handle_read_fault(pa_uaddress,physpage) < 0)
                  return FALSE;
              }
            if (!(physpage->protection == PROT_READ_WRITE) && (prot == PROT_READ_WRITE))
              # protection: PROT_READ -> PROT_READ_WRITE
              { if (handle_readwrite_fault(pa_uaddress,physpage) < 0)
                  return FALSE;
              }
    }     }
    return TRUE;
    #else
    return did_pagein;
    #endif
  }}
#endif

#ifdef SELFMADE_MMAP

local int selfmade_mmap (Heap* heap, uintL map_len, uintL offset);
local int selfmade_mmap(heap,map_len,offset)
  var Heap* heap;
  var uintL map_len;
  var uintL offset;
  { var uintL pagecount = map_len>>physpageshift;
    var uintB* pages = (uintB*)malloc(pagecount*sizeof(uintB));
    if (pages == NULL) { return -1; }
    heap->memfile_numpages = pagecount;
    heap->memfile_pages = pages;
    heap->memfile_offset = offset;
    # Initially, all pages are paged out.
    dotimespL(pagecount,pagecount, { *pages++ = 1; } );
    return 0;
  }

#endif

#ifdef GENERATIONAL_GC

local void xmprotect (aint addr, uintL len, int prot);
local void xmprotect(addr,len,prot)
  var aint addr;
  var uintL len;
  var int prot;
  { if (mprotect((MMAP_ADDR_T)addr,len,prot) < 0)
      { asciz_out(DEUTSCH ? "mprotect() klappt nicht." :
                  ENGLISH ? "mprotect() fails." :
                  FRANCAIS ? "mprotect() ne fonctionne pas." :
                  ""
                 );
        errno_out(OS_errno);
        abort();
  }   }

#ifdef MULTIMAP_MEMORY
  local void xmmprotect (Heap* heap, aint addr, uintL len, int prot);
  local void xmmprotect(heap,addr,len,prot)
    var Heap* heap;
    var aint addr;
    var uintL len;
    var int prot;
    { unused heap;
      { var uintL type;
        for (type = 0; type < typecount; type++)
          if (mem.heapnr_from_type[type] >= 0) # type in MM_TYPECASES aufgef�hrt?
            { xmprotect((aint)combine(type,addr),len,prot); }
      }
    }
#else
  #ifdef SELFMADE_MMAP
    local void xmmprotect (Heap* heap, aint addr, uintL len, int prot);
    local void xmmprotect(heap,addr,len,prot)
      var Heap* heap;
      var aint addr;
      var uintL len;
      var int prot;
      { # �berspringe die noch nicht eingeblendeten Seiten und mimimiere
        # dabei die Anzahl der n�tigen mprotect()-Aufrufe: Auf Halde steht
        # ein mprotect-Aufruf f�r das Intervall [todo_address,address-1].
        var aint todo_address = 0;
        #define do_todo()  \
          { if (todo_address)                                        \
              { if (todo_address < address)                          \
                  xmprotect(todo_address,address-todo_address,prot); \
                todo_address = 0;                                    \
          }   }
        #define addto_todo()  \
          { if (todo_address)             \
              {} # incrementiere address  \
              else                        \
              { todo_address = address; } \
          }
        var aint address = addr; # multiple of physpagesize
        var uintL pageno = (address - (heap->heap_gen0_start & -physpagesize)) >> physpageshift;
        var uintL pagecount = len >> physpageshift;
        var uintL count;
        dotimesL(count,pagecount,
          { if (!(pageno < heap->memfile_numpages && heap->memfile_pages[pageno]))
              { addto_todo(); }
              else
              { do_todo(); }
            address += physpagesize;
            pageno++;
          });
        do_todo();
        #undef addto_todo
        #undef do_todo
      }
  #else
    #define xmmprotect(heap,addr,len,prot)  xmprotect(addr,len,prot)
  #endif
#endif

#endif # GENERATIONAL_GC
