# Support for MULTIMAP_MEMORY model.

# ------------------------------ Specification ---------------------------------

# The operating system permits to use the same (virtual) memory areas under
# different addresses. There are some restrictions, however:
# - The mapping of addresses can be specified only for a memory page as a whole.
# - We need the address range only (not its contents), but nevertheless we have
#   malloc it and not give it back, because the address range shall remain
#   under our control. (We don't want subsequent malloc() calls to write data
#   into it.)

# The granularity of memory mapping, i.e. the length of a memory page.
# This is a power of two, usually 4096.
  local /* uintL */ aint map_pagesize;
# Initialize it.
# local void init_map_pagesize (void);

# Initialization:
# initmap() [MULTIMAP_MEMORY_VIA_SHM] or
# initmap(tmpdir) [MULTIMAP_MEMORY_VIA_FILE]
# Returns 0 is successful, -1 on error.

# Covers the range [map_addr,map_addr+map_len-1] with empty pages.
# (map_addr and map_len must be multiples of map_pagesize.)
# zeromap(map_addr,map_len)
# Returns 0 is successful, -1/errno on error.

# Covers the range [map_addr,map_addr+map_len-1] with pages, which shall
# be accessible at the typecodes enumerated in typecases.
# multimap(typecases,map_addr,map_len,save_flag);

# Number of possible typecodes.
# mm_types_count

# The list of typecodes satisfying !gcinvariant_type_p(type).
# MM_TYPECASES

# Combines a typecode and an address.
# combine(type,addr)

#ifdef HAVE_WORKING_MPROTECT
# Changes the memory protection of the range [map_addr,map_addr+map_len-1].
# mprotect(map_addr,map_len,PROT_{NONE,READ,READ_WRITE})
# Returns 0 is successful, -1/errno on error.
#endif

# Clean up and finish.
# exitmap();

# ------------------------------ Implementation --------------------------------

  #define mm_types_count  typecount

  #define MM_TYPECASES  \
    case_array: \
    case_closure: _case_structure _case_stream case_orecord: case_instance: \
    case_system: \
    case_bignum: case_ratio: case_ffloat: case_dfloat: case_lfloat: case_complex: \
    case_symbolflagged: case_cons:

  # Normal type+address combination.
  #define combine(type,addr)  ThePointer(type_pointer_object(type,addr))


#ifdef MULTIMAP_MEMORY_VIA_FILE

  #define init_map_pagesize()  \
    { map_pagesize = getpagesize(); }

  # Debug level for tempfile:
  #   0 = remove file immediately         "lisptemp.mem"
  #   1 = filename depends on process id  "lisptemp.mem.pid.1"
  #   2 = reuse file next time            "lisptemp.mem.1"
  #define TEMPFILE_DEBUG_LEVEL  0

  local char tempfilename[MAXPATHLEN]; # Name eines temporären Files
  local int zero_fd = -1; # Handle von /dev/zero
  # Zugriff auf /dev/zero: /dev/zero hat manchmal Permissions 0644. Daher
  # OPEN() mit nur O_RDONLY statt O_RDWR. Daher MAP_PRIVATE statt MAP_SHARED.

  local int initmap (const char* tmpdir);
  local int initmap(tmpdir)
    var const char* tmpdir;
    # Virtual Memory Mapping aufbauen:
    { # Wir brauchen ein temporäres File.
      # tempfilename := (string-concat tmpdir "/" "lisptemp.mem")
      {var const char* ptr1 = tmpdir;
       var char* ptr2 = &tempfilename[0];
       while (!(*ptr1 == '\0')) { *ptr2++ = *ptr1++; }
       if (!((ptr2 > &tempfilename[0]) && (ptr2[-1] == '/')))
         { *ptr2++ = '/'; }
       ptr1 = "lisptemp.mem";
       while (!(*ptr1 == '\0')) { *ptr2++ = *ptr1++; }
       #if (TEMPFILE_DEBUG_LEVEL > 0)
       *ptr2++ = '.';
       #if (TEMPFILE_DEBUG_LEVEL == 1)
       { unsigned int pid = getpid();
         *ptr2++ = ((pid >> 12) & 0x0f) + 'a';
         *ptr2++ = ((pid >> 8) & 0x0f) + 'a';
         *ptr2++ = ((pid >> 4) & 0x0f) + 'a';
         *ptr2++ = (pid & 0x0f) + 'a';
       }
       #endif
       *ptr2++ = '0';
       #endif
       *ptr2 = '\0';
      }
      { var int fd = OPEN("/dev/zero",O_RDONLY,my_open_mask);
        if (fd<0)
          { asciz_out(GETTEXT("Cannot open /dev/zero ."));
            errno_out(errno);
            return -1; # error
          }
        zero_fd = fd;
      }
      return 0;
    }

  #ifdef HAVE_MSYNC
    typedef struct { void* mm_addr; uintL mm_len; } mmap_interval;
    local mmap_interval mmap_intervals[256]; # 256 ist reichlich.
    local mmap_interval* mmap_intervals_ptr = &mmap_intervals[0];
    local void remember_mmap_interval (void* map_addr, uintL map_len);
    local void remember_mmap_interval(map_addr,map_len)
      var void* map_addr;
      var uintL map_len;
      { if (mmap_intervals_ptr == &mmap_intervals[256]) { abort(); }
        mmap_intervals_ptr->mm_addr = map_addr; mmap_intervals_ptr->mm_len = map_len;
        mmap_intervals_ptr++;
      }
    local void msync_mmap_intervals (void);
    local void msync_mmap_intervals()
      { var mmap_interval* ptr = &mmap_intervals[0];
        until (ptr==mmap_intervals_ptr)
          { if (msync((MMAP_ADDR_T)ptr->mm_addr,ptr->mm_len,MS_INVALIDATE) < 0)
              { asciz_out_2(GETTEXT("msync(0x%x,0x%x,MS_INVALIDATE) fails."),
                            ptr->mm_addr, ptr->mm_len
                           );
                errno_out(errno);
              }
            ptr++;
      }   }
  #else
     #define remember_mmap_interval(map_addr,map_len)
     #define msync_mmap_intervals()
  #endif

  local int fdmap (int fd, void* map_addr, uintL map_len, int readonly, int shared, int remember);
  local int fdmap(fd,map_addr,map_len,readonly,shared,remember)
    var int fd;
    var void* map_addr;
    var uintL map_len;
    var int readonly;
    var int shared;
    var int remember;
    { if ( (void*) mmap((MMAP_ADDR_T)map_addr, # gewünschte Adresse
                        map_len, # Länge
                        readonly ? PROT_READ : PROT_READ_WRITE, # Zugriffsrechte
                        (shared ? MAP_SHARED : 0) | MAP_FIXED, # genau an diese Adresse!
                        fd, 0 # File ab Position 0 legen
                       )
           == (void*)(-1)
         )
        { asciz_out_1(GETTEXT("Cannot map memory to address 0x%x ."),
                      map_addr
                     );
          errno_out(errno);
          return -1; # error
        }
      #ifdef HAVE_MSYNC
      if (remember) { remember_mmap_interval(map_addr,map_len); }
      #endif
      return 0;
    }

  local int zeromap (void* map_addr, uintL map_len);
  local int zeromap(map_addr,map_len)
    var void* map_addr;
    var uintL map_len;
    { return fdmap(zero_fd,map_addr,map_len,FALSE,FALSE,FALSE); }

  local int open_temp_fd (uintL map_len);
  local int open_temp_fd(map_len)
    var uintL map_len;
    { var int fd;
      #if (TEMPFILE_DEBUG_LEVEL > 0)
      tempfilename[strlen(tempfilename)-1]++;
      #endif
      #if (TEMPFILE_DEBUG_LEVEL <= 1)
      fd = OPEN(tempfilename,O_RDWR|O_CREAT|O_TRUNC|O_EXCL,my_open_mask);
      #else
      fd = OPEN(tempfilename,O_RDWR|O_CREAT,my_open_mask);
      #endif
      if (fd<0)
        { asciz_out_s(GETTEXT("Cannot open %s ."),
                      tempfilename
                     );
          errno_out(errno);
          return -1; # error
        }
      #if (TEMPFILE_DEBUG_LEVEL == 0)
      # und öffentlich unzugänglich machen, indem wir es löschen:
      # (Das Betriebssystem löscht das File erst dann, wenn am Ende dieses
      # Prozesses in _exit() ein close(fd) durchgeführt wird.)
      if ( unlink(tempfilename) <0)
        { asciz_out_s(GETTEXT("Cannot delete %s ."),
                      tempfilename
                     );
          errno_out(errno);
          return -1; # error
        }
      #endif
      # überprüfen, ob genug Plattenplatz da ist:
      { var struct statfs statbuf;
        if (!( fstatfs(fd,&statbuf) <0))
          if (!(statbuf.f_bsize == (long)(-1)) && !(statbuf.f_bavail == (long)(-1)))
            { var uintL available = (uintL)(statbuf.f_bsize) * (uintL)(statbuf.f_bavail);
              if (available < map_len)
                # auf der Platte ist voraussichtlich zu wenig Platz
                { asciz_out_s(GETTEXT("** WARNING: ** Too few free disk space for %s ." NLstring),
                             tempfilename
                             );
                  asciz_out(GETTEXT("Please restart LISP with fewer memory (option -m)." NLstring));
      }     }   }
      # Auf Größe map_len aufblähen:
      { var uintB dummy = 0;
        if (( lseek(fd,map_len-1,SEEK_SET) <0) || (!( full_write(fd,&dummy,1) ==1)))
          { asciz_out_s(GETTEXT("Cannot make %s long enough."),
                        tempfilename
                       );
            errno_out(errno);
            return -1; # error
      }   }
      return fd;
    }

  #if !defined(MAP_MEMORY_TABLES)
    # Kopiert den Inhalt des Intervalls [map_addr..map_addr+map_len-1] ins File.
    local int fdsave (int fd, void* map_addr, uintL map_len);
    local int fdsave(fd,map_addr,map_len)
      var int fd;
      var void* map_addr;
      var uintL map_len;
      { if (( lseek(fd,0,SEEK_SET) <0) || (!( full_write(fd,map_addr,map_len) == map_len)))
          { asciz_out_s(GETTEXT("Cannot fill %s ."),
                        tempfilename
                       );
            errno_out(errno);
            return -1; # error
          }
        return 0;
      }
  #else
    #define fdsave(fd,map_addr,map_len)  0
  #endif

  local int close_temp_fd (int fd);
  local int close_temp_fd(fd)
    var int fd;
    { if ( CLOSE(fd) <0)
        { asciz_out_s(GETTEXT("Cannot close %s ."),
                      tempfilename
                     );
          errno_out(errno);
          return -1; # error
        }
      return 0;
    }

  # Vorgehen bei multimap:
  # 1. Temporäres File aufmachen
    #define open_mapid(map_len)  open_temp_fd(map_len) # -> fd
  # 2. File mehrfach überlagert in den Speicher legen
    #define map_mapid(fd,map_addr,map_len,readonly)  fdmap(fd,map_addr,map_len,readonly,TRUE,TRUE)
  # 3. File schließen
  # (Das Betriebssystem schließt und löscht das File erst dann, wenn am
  # Ende dieses Prozesses in _exit() ein munmap() durchgeführt wird.)
    #define close_mapid(fd)  close_temp_fd(fd)

  #define multimap1(type,typecases,mapid,map_addr,map_len)  \
    { switch (type)        \
        { typecases        \
            if ( map_mapid(mapid,combine(type,map_addr),map_len,FALSE) <0) \
              goto no_mem; \
            break;         \
          default: break;  \
    }   }

  #define done_mapid(mapid,map_addr,map_len)  \
    if ( close_mapid(mapid) <0) \
      goto no_mem;
  #define exitmap()  \
    msync_mmap_intervals();                               \
    if (zero_fd >= 0)                                     \
      if ( CLOSE(zero_fd) <0)                             \
        { asciz_out(GETTEXT("Cannot close /dev/zero .")); \
          errno_out(errno);                               \
        }

  #define multimap(typecases,map_addr,map_len,save_flag)  \
    { # Temporäres File aufmachen:                            \
      var int mapid = open_mapid(map_len);                    \
      if (mapid<0) goto no_mem;                               \
      if (save_flag) { if ( fdsave(mapid,(void*)map_addr,map_len) <0) goto no_mem; } \
      # und mehrfach überlagert in den Speicher legen:        \
      { var oint type;                                        \
        for (type=0; type < mm_types_count; type++)           \
          { multimap1(type,typecases,mapid,map_addr,map_len); } \
      }                                                       \
      # und evtl. öffentlich unzugänglich machen:             \
      done_mapid(mapid,map_addr,map_len);                     \
    }

#endif # MULTIMAP_MEMORY_VIA_FILE


#ifdef MULTIMAP_MEMORY_VIA_SHM

# Virtual Memory Mapping über Shared Memory aufbauen:

  #define init_map_pagesize()  \
    { map_pagesize = SHMLBA; }

  local int initmap (void);
  local int initmap()
    {
     #ifdef UNIX_LINUX
      { var struct shminfo shminfo;
        if ( shmctl(0,IPC_INFO,(struct shmid_ds *)&shminfo) <0)
          if (errno==ENOSYS)
            { asciz_out(GETTEXT("Recompile your operating system with SYSV IPC support." NLstring));
              return -1; # error
      }     }
     #endif
     return 0;
    }

  local int open_shmid (uintL map_len);
  local int open_shmid(map_len)
    var uintL map_len;
    { var int shmid = shmget(IPC_PRIVATE,map_len,0700|IPC_CREAT); # 0700 = 'Read/Write/Execute nur für mich'
      if (shmid<0)
        { asciz_out(GETTEXT("Cannot allocate private shared memory segment."));
          errno_out(errno);
          return -1; # error
        }
      return shmid;
    }

  #ifndef SHM_REMAP  # Nur UNIX_LINUX benötigt SHM_REMAP in den shmflags
    #define SHM_REMAP  0
  #endif
  local int idmap (int shmid, void* map_addr, int shmflags);
  local int idmap(shmid,map_addr,shmflags)
    var int shmid;
    var void* map_addr;
    var int shmflags;
    { if ( shmat(shmid,
                 map_addr, # Adresse
                 shmflags # Flags (Default: Read/Write)
                )
           == (void*)(-1)
         )
        { asciz_out_1(GETTEXT("Cannot map shared memory to address 0x%x."),
                      map_addr
                     );
          errno_out(errno);
          return -1; # error
        }
      return 0;
    }

  #if !defined(MAP_MEMORY_TABLES)
    # Kopiert den Inhalt des Intervalls [map_addr..map_addr+map_len-1] ins
    # Shared-Memory-Segment.
    local int shmsave (int shmid, void* map_addr, uintL map_len);
    local int shmsave(shmid,map_addr,map_len)
      var int shmid;
      var void* map_addr;
      var uintL map_len;
      { var void* temp_addr = shmat(shmid,
                                         0, # Adresse: beliebig
                                         0 # Flags: brauche keine
                                        );
        if (temp_addr == (void*)(-1))
          { asciz_out(GETTEXT("Cannot fill shared memory."));
            errno_out(errno);
            return -1; # error
          }
        memcpy(temp_addr,map_addr,map_len);
        if (shmdt(temp_addr) < 0)
          { asciz_out(GETTEXT("Could not fill shared memory."));
            errno_out(errno);
            return -1; # error
          }
        return 0;
      }
  #else
    #define shmsave(shmid,map_addr,map_len)  0
  #endif

  local int close_shmid (int shmid);
  local int close_shmid(shmid)
    var int shmid;
    { if ( shmctl(shmid,IPC_RMID,NULL) <0)
        { asciz_out(GETTEXT("Cannot remove shared memory segment."));
          errno_out(errno);
          return -1; # error
        }
      return 0;
    }

  local int zeromap (void* map_addr, uintL map_len);
  local int zeromap(map_addr,map_len)
    var void* map_addr;
    var uintL map_len;
    { var int shmid = open_shmid(map_len);
      if (shmid<0)
        { return -1; } # error
      if (idmap(shmid,map_addr,0) < 0)
        { return -1; } # error
      return close_shmid(shmid);
    }

  # Vorgehen bei multimap:
  # 1. Shared-Memory-Bereich zur Verfügung stellen
    #define open_mapid(map_len)  open_shmid(map_len) # -> shmid
  # 2. Shared-Memory mehrfach überlagert in den Speicher legen
    #define map_mapid(shmid,map_addr,map_len,flags)  idmap(shmid,map_addr,flags)
  # 3. öffentlich unzugänglich machen, indem wir ihn löschen:
  # (Das Betriebssystem löscht den Shared Memory erst dann, wenn am
  # Ende dieses Prozesses in _exit() ein munmap() durchgeführt wird.)
    #define close_mapid(shmid)  close_shmid(shmid)

  #define multimap1(type,typecases,mapid,map_addr,map_len)  \
    { switch (type)                                  \
        { typecases                                  \
            if ( map_mapid(mapid, combine(type,map_addr), map_len, \
                           (type==0 ? SHM_REMAP : 0) \
                          )                          \
                 <0                                  \
               )                                     \
              goto no_mem;                           \
            break;                                   \
          default: break;                            \
    }   }

  #define done_mapid(mapid,map_addr,map_len)  \
    if ( close_mapid(mapid) <0) \
      goto no_mem;
  #define exitmap()

  #define multimap(typecases,total_map_addr,total_map_len,save_flag)  \
    { var uintL remaining_len = total_map_len;                                     \
      var aint map_addr = total_map_addr;                                          \
      do { var uintL map_len = (remaining_len > SHMMAX ? SHMMAX : remaining_len);  \
           # Shared-Memory-Bereich aufmachen:                                      \
           var int mapid = open_mapid(map_len);                                    \
           if (mapid<0) goto no_mem;                                               \
           if (save_flag && (map_addr==total_map_addr))                            \
             { if ( shmsave(mapid,(void*)total_map_addr,total_map_len) <0) goto no_mem; } \
           # und mehrfach überlagert in den Speicher legen:                        \
           { var oint type;                                                        \
             for (type=0; type < mm_types_count; type++)                           \
               { multimap1(type,typecases,mapid,map_addr,map_len); }               \
           }                                                                       \
           # und evtl. öffentlich unzugänglich machen:                             \
           done_mapid(mapid,map_addr,map_len);                                     \
           map_addr += map_len; remaining_len -= map_len;                          \
         }                                                                         \
         until (remaining_len==0);                                                 \
    }

#endif # MULTIMAP_MEMORY_VIA_SHM
