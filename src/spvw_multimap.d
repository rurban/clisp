/* Support for MULTIMAP_MEMORY model. */

/* -------------------------- Specification ---------------------------- */

/* The operating system permits to use the same (virtual) memory areas under
 different addresses. There are some restrictions, however:
 - The mapping of addresses can be specified only for a memory page as a whole.
 - We need the address range only (not its contents), but nevertheless we have
   malloc it and not give it back, because the address range shall remain
   under our control. (We don't want subsequent malloc() calls to write data
   into it.)

 The granularity of memory mapping, i.e. the length of a memory page.
 This is a power of two, usually 4096. */
local /* uintL */ aint map_pagesize;
/* Initialize it.
 local void init_map_pagesize (void);

 Initialization:
 initmap() [MULTIMAP_MEMORY_VIA_SHM]
 Returns 0 is successful, -1 on error.

 Covers the range [map_addr,map_addr+map_len-1] with empty pages.
 (map_addr and map_len must be multiples of map_pagesize.)
 zeromap(map_addr,map_len)
 Returns 0 is successful, -1/errno on error.

 Covers the range [map_addr,map_addr+map_len-1] with pages, which shall
 be accessible at the typecodes enumerated in typecases.
 multimap(typecases,map_addr,map_len,save_flag);

 Number of possible typecodes.
 mm_types_count

 The list of typecodes satisfying !gcinvariant_type_p(type).
 MM_TYPECASES

 Combines a typecode and an address.
 combine(type,addr) */

#ifdef HAVE_WORKING_MPROTECT
/* Changes the memory protection of the range [map_addr,map_addr+map_len-1].
 mprotect(map_addr,map_len,PROT_{NONE,READ,READ_WRITE})
 Returns 0 is successful, -1/errno on error. */
#endif

/* Clean up and finish.
 exitmap(); */

/* -------------------------- Implementation --------------------------- */

#define mm_types_count  typecount

#define MM_TYPECASES                            \
  case_array:                                                           \
  case_closure: _case_structure _case_stream case_orecord: case_instance: \
  case_lrecord:                                                         \
  case_system:                                                          \
  case_bignum: case_ratio: case_ffloat: case_dfloat: case_lfloat: case_complex: \
  case_symbolflagged: case_cons:

/* Normal type+address combination. */
#define combine(type,addr)  ThePointer(type_pointer_object(type,addr))


#ifdef MULTIMAP_MEMORY_VIA_SHM

/* build up virtual memory mapping via shared memory: */

#define init_map_pagesize()    { map_pagesize = SHMLBA; }

local int initmap (void)
{
 #ifdef UNIX_LINUX
  {
    var struct shminfo shminfo;
    if ( shmctl(0,IPC_INFO,(struct shmid_ds *)&shminfo) <0)
      if (errno==ENOSYS) {
        fprintf(stderr,GETTEXTL("Recompile your operating system with SYSV IPC support."));
        fputs("\n",stderr);
        return -1;              /* error */
      }
  }
 #endif
  return 0;
}

local int open_shmid (uintM map_len)
{
  var int shmid = shmget(IPC_PRIVATE,map_len,0700|IPC_CREAT); /* 0700 = 'Read/Write/Execute only for me' */
  if (shmid<0) {
    fprintf(stderr,GETTEXTL("Cannot allocate private shared memory segment of size %d."),map_len);
    errno_out(errno);
    return -1;                  /* error */
  }
  return shmid;
}

  #ifndef SHM_REMAP /* Only UNIX_LINUX needs SHM_REMAP in the shmflags */
    #define SHM_REMAP  0
  #endif
local int idmap (int shmid, void* map_addr, int shmflags)
{
  if (shmat(shmid,map_addr/*address*/,shmflags/*flags(default: read/write)*/)
      == (void*)(-1)) {
    fprintf(stderr,GETTEXTL("Cannot map shared memory to address 0x%lx."),
            map_addr);
    errno_out(errno);
    return -1;                /* error */
  }
  return 0;
}

#if !defined(MAP_MEMORY_TABLES)
/* copies the content of the interval [map_addr..map_addr+map_len-1] into
   the shared-memory-segment. */
local int shmsave (int shmid, void* map_addr, uintM map_len)
{
  var void* temp_addr = shmat(shmid,0/*address: arbitrary*/,0/*flags: none*/);
  if (temp_addr == (void*)(-1)) {
    fprintf(stderr,GETTEXTL("%s: Cannot fill shared memory."),"shmat");
    errno_out(errno);
    return -1;                  /* error */
  }
  memcpy(temp_addr,map_addr,map_len);
  if (shmdt(temp_addr) < 0) {
    fprintf(stderr,GETTEXTL("%s: Cannot fill shared memory."),"shmdt");
    errno_out(errno);
    return -1;                  /* error */
  }
  return 0;
}
#else
  #define shmsave(shmid,map_addr,map_len)  0
#endif

local int close_shmid (int shmid)
{
  if ( shmctl(shmid,IPC_RMID,NULL) <0) {
    fprintf(stderr,GETTEXTL("Cannot remove shared memory segment."));
    errno_out(errno);
    return -1;                  /* error */
  }
  return 0;
}

local int zeromap (void* map_addr, uintM map_len)
{
  var int shmid = open_shmid(map_len);
  if (shmid<0)
    return -1;                  /* error */
  if (idmap(shmid,map_addr,0) < 0)
    return -1;                  /* error */
  return close_shmid(shmid);
}

/* procedure for multimap:
   1. make shared-memory-region available */
#define open_mapid(map_len)  open_shmid(map_len) /* -> shmid */
/* 2. put shared-memory multimapped into the memory */
#define map_mapid(shmid,map_addr,map_len,flags)  idmap(shmid,map_addr,flags)
/* 3. make publicly inaccessible by deleting it:
   (the operating system will delete the shared memory only when at the end
   of this process in _exit() a munmap() is performed.) */
#define close_mapid(shmid)  close_shmid(shmid)

#define multimap1(type,typecases,mapid,map_addr,map_len)         \
  { switch (type) {                                              \
    typecases                                                    \
      if ( map_mapid(mapid, combine(type,map_addr), map_len,     \
                     (type==0 ? SHM_REMAP : 0))                  \
           <0)                                                   \
        goto no_mem;                                             \
    break;                                                       \
      default: break;                                            \
  }}

#define done_mapid(mapid,map_addr,map_len)      \
  if ( close_mapid(mapid) <0)                   \
    goto no_mem;
#define exitmap()

#define multimap(typecases,total_map_addr,total_map_len,save_flag)      \
  { var uintM remaining_len = total_map_len;                            \
    var aint map_addr = total_map_addr;                                 \
    do {                                                                \
      var uintM map_len = (remaining_len > SHMMAX ? SHMMAX : remaining_len); \
      /* open shared-memory-region: */                                  \
      var int mapid = open_mapid(map_len);                              \
      if (mapid<0) goto no_mem;                                         \
      if (save_flag && (map_addr==total_map_addr)) {                    \
        if ( shmsave(mapid,(void*)total_map_addr,total_map_len) <0)     \
          goto no_mem;                                                  \
      }                                                                 \
      /* and put multimapped into the memory: */                        \
      { var oint type;                                                  \
        for (type=0; type < mm_types_count; type++) {                   \
          multimap1(type,typecases,mapid,map_addr,map_len);             \
        }                                                               \
      }                                                                 \
      /* and poss. make publicly inaccessible: */                       \
      done_mapid(mapid,map_addr,map_len);                               \
      map_addr += map_len; remaining_len -= map_len;                    \
    } while (remaining_len!=0);                                         \
  }

#endif  /* MULTIMAP_MEMORY_VIA_SHM */
