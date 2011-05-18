/* Handling of signal SIGCLD. */

/* -------------------------- Specification ----------------------------- */

#ifdef HAVE_SIGNALS

/* Install the signal handler for SIGCLD. */
local void install_sigcld_handler (void);

#endif

/* -------------------------- Implementation ---------------------------- */

#ifdef HAVE_SIGNALS

/* Our general policy with child processes - in particular child processes
 to which we are connected through pipes - is not to wait for them, but
 instead do what init(1) would do in case our process terminates before
 the child: perform a non-blocking waitpid() and ignore the child's
 termination status.
   void handle_child () { while (waitpid(-1,NULL,WNOHANG) > 0); }
   SIGNAL(SIGCLD,handle_child);
 The following is equivalent (but better, since it doesn't interrupt system
 calls):
   SIGNAL(SIGCLD,SIG_IGN);
 However, sometimes we may want to enable SIGCLD
 (see syscalls/posix.lisp:with-subprocesses)
 in such a way that the end_want_sigcld at the end of LAUNCH does not cause
 a race condition, thus sigcld_enabled counts how many times we have enabled
 the signal. */

local int sigcld_enabled = 0;
#if defined(MULTITHREAD)
/* TODO: following should be just atomic increment/decrement.
   should add atomic primitives to xthread.d  */
local spinlock_t sigcld_enabled_lock;
#define sigcld_enabled_INCF(v)                  \
  spinlock_acquire(&sigcld_enabled_lock);       \
  v = sigcld_enabled++;                         \
  spinlock_release(&sigcld_enabled_lock)
#define sigcld_enabled_DECF(v)                  \
  spinlock_acquire(&sigcld_enabled_lock);       \
  if (sigcld_enabled)                           \
    v = --sigcld_enabled;                       \
  else v = 0;                                   \
  spinlock_release(&sigcld_enabled_lock)
#else
#define sigcld_enabled_INCF(v)  v = sigcld_enabled++
#define sigcld_enabled_DECF(v)  v = --sigcld_enabled
#endif

local void install_sigcld_handler (void) {
 #if defined(MULTITHREAD)
  spinlock_init(&sigcld_enabled_lock);
 #endif
 #if defined(SIGCLD)
  SIGNAL(SIGCLD,SIG_IGN);
 #endif
}

modexp void begin_want_sigcld () {
  int sigcld_level;
  sigcld_enabled_INCF(sigcld_level);
 #if defined(SIGCLD)
  if (sigcld_level == 0)
    SIGNAL(SIGCLD,SIG_DFL);
 #endif
}

modexp void end_want_sigcld () {
  int sigcld_level;
  sigcld_enabled_DECF(sigcld_level);
 #if defined(SIGCLD)
  if (sigcld_level == 0) {
    SIGNAL(SIGCLD,SIG_IGN);
    /* Try to remove zombies which may have been created since the last
       begin_want_sigcld() call. */
    while (waitpid(-1,NULL,WNOHANG) > 0) { fprintf(stderr, "VTZ: waitpid() iteration"); }
  }
 #endif
}

#endif
