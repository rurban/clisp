/* Test the stack overflow handler. */

#include "sigsegv.h"

#ifdef HAVE_STACK_OVERFLOW_RECOVERY

#ifndef _WIN32
#include "config.h"
#endif

#include <stddef.h> /* needed for NULL on SunOS4 */
#include <stdlib.h> /* for abort, exit */
#include <stdio.h> /* for printf */

#include <setjmp.h>
jmp_buf mainloop;

#include <signal.h>

#ifdef HAVE_SETRLIMIT
#include <sys/types.h>
#include <sys/time.h>
#include <sys/resource.h>
#endif

int pass = 0;

void stackoverflow_handler (int emergency, stackoverflow_context_t scp)
{
  pass++;
  printf("Stack overflow %d caught.\n",pass);
  sigsegv_leave_handler();
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

#if defined(HAVE_SETRLIMIT) && defined(RLIMIT_STACK)
  /* Before starting the endless recursion, try to be friendly to the user's
     machine. If you install a Linux 2.2.x kernel on a SuSE 5.3 Linux system,
     you end up with no stack limit for user processes at all. We don't want
     to kill such systems. */
  struct rlimit rl;
  rl.rlim_cur = rl.rlim_max = 0x100000; /* 1 MB */
  setrlimit(RLIMIT_STACK,&rl);
#endif

  if (stackoverflow_install_handler(&stackoverflow_handler,mystack,sizeof(mystack)) < 0)
    exit(0);

  switch (setjmp(mainloop)) {
    case -1:
      printf("emergency exit\n"); exit(1);
    case 0: case 1:
      printf("Starting recursion pass %d.\n",pass+1);
      recurse(0);
      printf("no endless recursion?!\n"); exit(1);
    case 2:
      printf("Test passed.\n"); exit(0);
    default:
      abort();
  }
}

#else

int main ()
{
  return 0;
}

#endif
