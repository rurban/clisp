# Auxiliary functions for CLISP on Acorn RISC OS
# Peter Burwood 30.11.1996

# Do not include lispbibl.c because of header file problems.

#define global
#define local static
#define var

#ifdef __riscos

# Workaround for weird signal/longjmp interaction in UnixLib 3.7b.
# Peter calls this "broken behaviour". Bruno says this kind of behaviour
# is allowed according to the ANSI C standard.

# This is a nasty hack until the new signal handling in UnixLib fixes a problem
# with longjmp'ing out of signal handlers. Currently, the signal system
# has a flag indicating whether the signal handler is already active so
# that new signals are placed in the pending queue. When the signal handler
# returns the this flag is cleared. When the signal handler executes longjmp
# the flag is not cleared. This is broken behaviour, but the fix is not easy.

# Undefine __JMP_BUF_SIZE so the library defined version in sys/unix.h is used.
# This is necessary when this file is compiled with gcc which predefines
# __JMP_BUF_SIZE on RISC OS because UnixLib is compiled with Norcroft C and
# the __JMP_BUF_SIZE defaults to a different value.
# The reason that this is important is that the proc struct has an embedded
# jmp_buf and the field we are altering below appears after the jmp_buf.

#ifdef __JMP_BUF_SIZE
#undef __JMP_BUF_SIZE
#endif

#include <sys/unix.h>
#include <signal.h>

global void prepare_signal_handler_exit (sig)
  var int sig;
{ sigset_t sigblock_mask;
  sigemptyset(&sigblock_mask);
  sigaddset(&sigblock_mask,sig);
  __u->sigstate.currently_handling = 0;
  sigprocmask(SIG_UNBLOCK,&sigblock_mask,NULL);
}

#endif # __riscos
