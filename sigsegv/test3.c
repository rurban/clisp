/* Test the stack overflow handler. */

#include "sigsegv.h"

#ifdef HAVE_STACK_OVERFLOW_RECOVERY

#ifndef _WIN32
#include "config.h"
#endif

#include <stddef.h> /* needed for NULL on SunOS4 */

#include <setjmp.h>
jmp_buf mainloop;

#include <signal.h>

int pass = 0;

void stackoverflow_handler (int emergency)
{
  pass++;
  printf("Stack overflow %d caught.\n",pass);
#if (defined(HAVE_SIGACTION) ? defined(SIGACTION_NEED_UNBLOCK) : defined(SIGNAL_NEED_UNBLOCK))
#if defined(SIGNALBLOCK_POSIX)
  {
    sigset_t sigblock_mask;
    sigemptyset(&sigblock_mask);
    sigaddset(&sigblock_mask,SIGSEGV);
#ifdef SIGBUS
    sigaddset(&sigblock_mask,SIGBUS);
#endif
    sigprocmask(SIG_UNBLOCK,&sigblock_mask,NULL);
  }
#elif defined(SIGNALBLOCK_BSD)
  {
    long sigblock_mask = sigblock(0);
    sigblock_mask &= ~sigmask(SIGSEGV);
#ifdef SIGBUS
    sigblock_mask &= ~sigmask(SIGBUS);
#endif
    sigsetmask(sigblock_mask);
  }
#endif
#endif
  longjmp(mainloop, emergency ? -1 : pass);
}

int recurse (int n)
{
  if (n >= 0)
    return n + recurse(n+1);
  else
    return 0;
}

int main ()
{
  char mystack[16384];
  stackoverflow_install_handler(&stackoverflow_handler,mystack,sizeof(mystack));
  switch (setjmp(mainloop)) {
    case -1: printf("emergency exit\n"); exit(1);
    case 0: case 1: recurse(0); printf("no endless recursion?!\n"); exit(1);
    case 2: printf("Test passed.\n"); exit(0);
    default: abort();
  }
}

#else

int main ()
{
  return 0;
}

#endif
