# GC statistics.

# ------------------------------ Specification ---------------------------------

# Number of GCs executed so far in this process.
  extern uintL gc_count;

# Increment the number of GCs so far.
# inc_gc_count();

# Number of bytes collected by GCs so far in this process.
  extern uintL2 gc_space;

# Accumulate the number of bytes collected by a GC.
# inc_gc_space(freed);

# Time used by GC so far in this process.
  extern internal_time gc_time;

# Toggle the GC timer on/off: gc_timer_on(); ... gc_timer_off();
# The time elapsed in between the two calls is added to gc_time.

# ------------------------------ Implementation --------------------------------

  global uintL gc_count = 0;

  #define inc_gc_count()  { gc_count++; }

  global uintL2 gc_space =
    #ifdef intQsize
      0
    #else
      {0,0}
    #endif
    ;
 
  #ifdef intQsize
    #define inc_gc_space(freed)  { gc_space += (uintL)(freed); }
  #else
    #define inc_gc_space(freed)  \
      { gc_space.lo += (uintL)(freed);                \
        if (gc_space.lo < (uintL)(freed)) # Übertrag? \
          gc_space.hi += 1;                           \
      }
  #endif

  global internal_time gc_time =
    #ifdef TIME_1
      0
    #endif
    #ifdef TIME_2
      {0,0}
    #endif
    ;

  #define gc_timer_on()  \
    { var internal_time gcstart_time; \
      get_running_time(gcstart_time); # aktuelle verbrauchte Zeit abfragen und retten
  #define gc_timer_off()  \
     {var internal_time gcend_time;                           \
      get_running_time(gcend_time);                           \
      # Differenz von gcend_time und gcstart_time bilden:     \
      sub_internal_time(gcend_time,gcstart_time, gcend_time); \
      # diese Differenz zu gc_time addieren:                  \
      add_internal_time(gc_time,gcend_time, gc_time);         \
    }}
