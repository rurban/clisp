/*
 * Copyright 1993-1998 Bruno Haible, <haible@clisp.cons.org>
 *
 * This is free software distributed under the GNU General Public Licence
 * described in the file COPYING. Contact the author if you don't have this
 * or can't live with it. There is ABSOLUTELY NO WARRANTY, explicit or implied,
 * on this software.
 */

#include "sigsegv.h"

#if defined(HAVE_SIGSEGV_RECOVERY)

#ifndef _WIN32
#include "config.h"
#endif

#include <stddef.h> /* needed for NULL on SunOS4 */
#include <stdlib.h>

/*
 * Portability section:
 * - SIGSEGV_FAULT_HANDLER_ARGLIST  is the argument list for the actual fault
 *                                  handler.
 * - SIGSEGV_FAULT_ADDRESS          is a macro for fetching the fault address.
 * - SIGSEGV_ALL_SIGNALS            enumerates the fault signals.
 */
#if defined(linux) && (defined(i386) || defined(__i386)) /* Linux */
#define SIGSEGV_FAULT_HANDLER_ARGLIST  int sig, unsigned long more
#define SIGSEGV_FAULT_ADDRESS  ((unsigned long *) &more) [21]
#define SIGSEGV_ALL_SIGNALS  FAULT_HANDLER(SIGSEGV)
#endif
#if defined(__NetBSD__) || defined(__FreeBSD__) /* NetBSD, FreeBSD */
#define SIGSEGV_FAULT_HANDLER_ARGLIST  int sig, int code, void* scp, char* addr
#define SIGSEGV_FAULT_ADDRESS  addr
#define SIGSEGV_ALL_SIGNALS  FAULT_HANDLER(SIGBUS)
#endif
#if defined(linux) && defined(sparc) /* Linux, in case of SunOS4 signal frames */
#define SIGSEGV_FAULT_HANDLER_ARGLIST  int sig, int code, void* scp, char* addr
#define SIGSEGV_FAULT_ADDRESS  addr
#define SIGSEGV_ALL_SIGNALS  FAULT_HANDLER(SIGSEGV)
#endif
#if defined(sun) && defined(unix) && (defined(mc68020) || defined(sparc) || (defined(i386) || defined(__i386))) && defined(HAVE_VADVISE) /* SunOS 4 */
#define SIGSEGV_FAULT_HANDLER_ARGLIST  int sig, int code, void* scp, char* addr
#define SIGSEGV_FAULT_ADDRESS  addr
#define SIGSEGV_ALL_SIGNALS  FAULT_HANDLER(SIGSEGV) FAULT_HANDLER(SIGBUS)
#endif
#if defined(sun) && defined(unix) && (defined(mc68020) || defined(sparc) || (defined(i386) || defined(__i386))) && !defined(HAVE_VADVISE) /* SunOS 5 */
#include <siginfo.h>
#define SIGSEGV_FAULT_HANDLER_ARGLIST  int sig, siginfo_t* sip, void* ucp
#define SIGSEGV_FAULT_ADDRESS  sip->si_addr
#define SIGSEGV_FAULT_ADDRESS_FROM_SIGINFO
#define SIGSEGV_ALL_SIGNALS  FAULT_HANDLER(SIGSEGV)
#endif
#if (defined(sgi) || defined(__sgi)) && (defined(SYSTYPE_SVR4) || defined(__SYSTYPE_SVR4)) /* Irix 5 */
#define SIGSEGV_FAULT_HANDLER_ARGLIST  int sig, int code, struct sigcontext *scp
#define SIGSEGV_FAULT_ADDRESS  scp->sc_badvaddr
#define SIGSEGV_ALL_SIGNALS  FAULT_HANDLER(SIGSEGV)
#endif
#if defined(__osf__) /* OSF/1 */
#define SIGSEGV_FAULT_HANDLER_ARGLIST  int sig, int code, struct sigcontext *scp
#define SIGSEGV_FAULT_ADDRESS  scp->sc_traparg_a0
#define SIGSEGV_ALL_SIGNALS  FAULT_HANDLER(SIGSEGV)
#endif
#if defined(_AIX) /* AIX */
#define SIGSEGV_FAULT_HANDLER_ARGLIST  int sig, int code, struct sigcontext *scp
#define SIGSEGV_FAULT_ADDRESS  scp->sc_jmpbuf.jmp_context.o_vaddr
#define SIGSEGV_ALL_SIGNALS  FAULT_HANDLER(SIGSEGV)
#endif
#if defined(NeXT) /* NeXTstep */
#endif
#if 0 /* Single Unix Specification, Version 2 */
#define SIGSEGV_FAULT_HANDLER_ARGLIST  int sig, siginfo_t* sip, void* ucp
#define SIGSEGV_FAULT_ADDRESS  sip->si_addr
#define SIGSEGV_FAULT_ADDRESS_FROM_SIGINFO
#define SIGSEGV_ALL_SIGNALS  FAULT_HANDLER(SIGSEGV)
#endif
#if defined(_WIN32) && !defined(__BORLANDC__) /* Win32, but not Borland C */
#endif

static int no_user_handler (void* fault_address)
{
  (void)fault_address;
  /* This shouldn't be called. Decline responsibility. */
  return 0;
}

/* User's SIGSEGV handler. */
static sigsegv_handler_t user_handler = &no_user_handler;

#if !(defined(NeXT) || defined(_WIN32))
/* Generic Unix support. */

/* Signal handling support. */
#include <signal.h>
#include <errno.h>
#ifdef __cplusplus
#ifdef SIGTYPE_DOTS
typedef RETSIGTYPE (*signal_handler) (...);
#else
typedef RETSIGTYPE (*signal_handler) (int);
#endif
#else
typedef RETSIGTYPE (*signal_handler) ();
#endif

/* Forward declaration. */
static void install_for (int sig);

#if !defined(HAVE_SIGACTION) && defined(EINTR) && !defined(HAVE_SIGINTERRUPT)
static int siginterrupt (int sig, int flag)
{
#if defined(HAVE_SIGVEC) && defined(SV_INTERRUPT)
  struct sigvec sv;
  sigvec(sig,(struct sigvec *)NULL,&sv);
  if (flag) {
    if (sv.sv_flags & SV_INTERRUPT) return 0;
    sv.sv_flags |= SV_INTERRUPT;
  } else {
    if (!(sv.sv_flags & SV_INTERRUPT)) return 0;
    sv.sv_flags &= ~ SV_INTERRUPT;
  }
  sigvec(sig,&sv,(struct sigvec *)NULL);
#endif
  return 0;
}
#endif

/* Our SIGSEGV handler, with OS dependent argument list. */
static void sigsegv_handler (SIGSEGV_FAULT_HANDLER_ARGLIST)
{
  char* address = (char*)(SIGSEGV_FAULT_ADDRESS);
  /* Call user's handler. */
  if ((*user_handler)(address)) {
    /* Handler successful. Reinstall it for next time. */
#if (defined(HAVE_SIGACTION) ? defined(SIGACTION_NEED_REINSTALL) : defined(SIGNAL_NEED_REINSTALL))
    install_for(sig);
#endif
  } else {
    /* Handler declined responsibility. Remove ourselves and dump core. */
    sigsegv_deinstall_handler();
  }
}

static void install_for (int sig)
{

#ifdef HAVE_SIGACTION

  struct sigaction action;
#if 0 /* Single Unix Specification, Version 2 */
  action.sa_sigaction = &sigsegv_handler;
#else
  action.sa_handler = (signal_handler)&sigsegv_handler;
#endif
  /* Block most signals while SIGSEGV is being handled. */
  /* Signals SIGKILL, SIGSTOP cannot be blocked. */
  /* Signals SIGCONT, SIGTSTP, SIGTTIN, SIGTTOU are not blocked because
     dealing with these signals seems dangerous. */
  /* Signals SIGILL, SIGABRT, SIGFPE, SIGSEGV, SIGTRAP, SIGIOT, SIGEMT, SIGBUS,
     SIGSYS, SIGSTKFLT are not blocked because these are synchronous signals,
     which may require immediate intervention, otherwise the process may
     starve. */
  sigemptyset(&action.sa_mask);
#ifdef SIGHUP
  sigaddset(&action.sa_mask,SIGHUP);
#endif
#ifdef SIGINT
  sigaddset(&action.sa_mask,SIGINT);
#endif
#ifdef SIGQUIT
  sigaddset(&action.sa_mask,SIGQUIT);
#endif
#ifdef SIGPIPE
  sigaddset(&action.sa_mask,SIGPIPE);
#endif
#ifdef SIGALRM
  sigaddset(&action.sa_mask,SIGALRM);
#endif
#ifdef SIGTERM
  sigaddset(&action.sa_mask,SIGTERM);
#endif
#ifdef SIGUSR1
  sigaddset(&action.sa_mask,SIGUSR1);
#endif
#ifdef SIGUSR2
  sigaddset(&action.sa_mask,SIGUSR2);
#endif
#ifdef SIGCHLD
  sigaddset(&action.sa_mask,SIGCHLD);
#endif
#ifdef SIGCLD
  sigaddset(&action.sa_mask,SIGCLD);
#endif
#ifdef SIGURG
  sigaddset(&action.sa_mask,SIGURG);
#endif
#ifdef SIGIO
  sigaddset(&action.sa_mask,SIGIO);
#endif
#ifdef SIGPOLL
  sigaddset(&action.sa_mask,SIGPOLL);
#endif
#ifdef SIGXCPU
  sigaddset(&action.sa_mask,SIGXCPU);
#endif
#ifdef SIGXFSZ
  sigaddset(&action.sa_mask,SIGXFSZ);
#endif
#ifdef SIGVTALRM
  sigaddset(&action.sa_mask,SIGVTALRM);
#endif
#ifdef SIGPROF
  sigaddset(&action.sa_mask,SIGPROF);
#endif
#ifdef SIGPWR
  sigaddset(&action.sa_mask,SIGPWR);
#endif
#ifdef SIGLOST
  sigaddset(&action.sa_mask,SIGLOST);
#endif
#ifdef SIGWINCH
  sigaddset(&action.sa_mask,SIGWINCH);
#endif
  /* Ask the OS to provide a structure siginfo_t to the handler. */
#ifdef SIGSEGV_FAULT_ADDRESS_FROM_SIGINFO
  action.sa_flags = SA_SIGINFO;
#else
  action.sa_flags = 0;
#endif
  sigaction(sig,&action,(struct sigaction *)NULL);

#else /* no HAVE_SIGACTION */

  signal(sig,&sigsegv_handler);
#ifdef EINTR
  siginterrupt(sig,0);
#endif

#endif
}

int sigsegv_install_handler (sigsegv_handler_t handler)
{
  user_handler = handler;
#define FAULT_HANDLER(sig)  install_for(sig);
  SIGSEGV_ALL_SIGNALS
#undef FAULT_HANDLER
  return 0;
}

void sigsegv_deinstall_handler (void)
{
#define FAULT_HANDLER(sig)  signal(sig,SIG_DFL);
  SIGSEGV_ALL_SIGNALS
#undef FAULT_HANDLER
  user_handler = &no_user_handler;
}

void sigsegv_leave_handler (void)
{
  /* Reinstall the handler. */
#if (defined(HAVE_SIGACTION) ? defined(SIGACTION_NEED_REINSTALL) : defined(SIGNAL_NEED_REINSTALL))
#define FAULT_HANDLER(sig)  install_for(sig);
  SIGSEGV_ALL_SIGNALS
#undef FAULT_HANDLER
#endif
  /* Unblock the signals that were blocked when the handler was entered. */
#ifdef HAVE_SIGACTION
  {
    sigset_t sigblock_mask;
    sigemptyset(&sigblock_mask);
#ifdef SIGHUP
    sigaddset(&sigblock_mask,SIGHUP);
#endif
#ifdef SIGINT
    sigaddset(&sigblock_mask,SIGINT);
#endif
#ifdef SIGQUIT
    sigaddset(&sigblock_mask,SIGQUIT);
#endif
#ifdef SIGPIPE
    sigaddset(&sigblock_mask,SIGPIPE);
#endif
#ifdef SIGALRM
    sigaddset(&sigblock_mask,SIGALRM);
#endif
#ifdef SIGTERM
    sigaddset(&sigblock_mask,SIGTERM);
#endif
#ifdef SIGUSR1
    sigaddset(&sigblock_mask,SIGUSR1);
#endif
#ifdef SIGUSR2
    sigaddset(&sigblock_mask,SIGUSR2);
#endif
#ifdef SIGCHLD
    sigaddset(&sigblock_mask,SIGCHLD);
#endif
#ifdef SIGCLD
    sigaddset(&sigblock_mask,SIGCLD);
#endif
#ifdef SIGURG
    sigaddset(&sigblock_mask,SIGURG);
#endif
#ifdef SIGIO
    sigaddset(&sigblock_mask,SIGIO);
#endif
#ifdef SIGPOLL
    sigaddset(&sigblock_mask,SIGPOLL);
#endif
#ifdef SIGXCPU
    sigaddset(&sigblock_mask,SIGXCPU);
#endif
#ifdef SIGXFSZ
    sigaddset(&sigblock_mask,SIGXFSZ);
#endif
#ifdef SIGVTALRM
    sigaddset(&sigblock_mask,SIGVTALRM);
#endif
#ifdef SIGPROF
    sigaddset(&sigblock_mask,SIGPROF);
#endif
#ifdef SIGPWR
    sigaddset(&sigblock_mask,SIGPWR);
#endif
#ifdef SIGLOST
    sigaddset(&sigblock_mask,SIGLOST);
#endif
#ifdef SIGWINCH
    sigaddset(&sigblock_mask,SIGWINCH);
#endif
    sigprocmask(SIG_UNBLOCK,&sigblock_mask,NULL);
  }
#else
#if defined(SIGNAL_NEED_UNBLOCK)
  {
    sigset_t sigblock_mask = sigblock(0);
#define FAULT_HANDLER(sig)  sigblock_mask &= ~sigmask(sig);
    SIGSEGV_ALL_SIGNALS
#undef FAULT_HANDLER
    sigsetmask(sigblock_mask);
  }
#endif
#if defined(SIGNAL_NEED_UNBLOCK_OTHERS)
  sigsetmask(0);
#endif
#endif
}

#endif /* Unix */

#if defined(NeXT) /* NeXTstep */

/*
 * We get the fault address as a subcode of a Mach exception. To get this,
 * we have a thread listening on the exception port.
 */

#include <mach/exception.h>
#include <mach/exc_server.h>
#include <mach/cthreads.h>

/*
 * The global exception handling function, called by exc_server().
 */
static int exception_handled = 0;
kern_return_t catch_exception_raise (port_t exception_port, port_t thread, port_t task, int exception, int code, int subcode)
{
  if (user_handler == &no_user_handler)
    return KERN_FAILURE;
  if ((exception == EXC_BAD_ACCESS)
      /*
       * see <mach/exception.h>:
       *   Could not access memory
       *   Code contains kern_return_t describing error.
       *   Subcode contains bad memory address.
       */
      && (*user_handler)((void*)subcode)) {
    exception_handled = 1;
    return KERN_SUCCESS;
  } else {
    exception_handled = 0;
    return KERN_FAILURE;
  }
}

static port_t main_thread_port;
static port_t old_exception_port;
static port_t new_exception_port;

static any_t exception_thread_main (void* dummy)
{
  char in_msg_data[excMaxRequestSize]; /* see <mach/exc_server.h> */
  char out_msg_data[excMaxReplySize]; /* see <mach/exc_server.h> */
#define in_msg  (*((msg_header_t*)&in_msg_data[0]))
#define out_msg  (*((msg_header_t*)&out_msg_data[0]))
  kern_return_t retval;
  for (;;) {
    /* Wait for a message on the exception port. */
    in_msg.msg_size = excMaxRequestSize;
    in_msg.msg_local_port = new_exception_port;
    retval = msg_receive(&in_msg,MSG_OPTION_NONE,0);
    if (!(retval==KERN_SUCCESS)) {
      printf("Mach msg_receive didn't succeed.\n");
      abort();
    }
    /* Call exception handler 1, yields an answer in out_msg. */
    if (!exc_server(&in_msg,&out_msg)) {
      printf("Mach exc_server didn't succeed.\n");
      abort();
    }
    /* Forward the answer. */
    retval = msg_send(&out_msg,MSG_OPTION_NONE,0);
    if (!(retval==KERN_SUCCESS)) {
      printf("Mach msg_send didn't succeed.\n");
      abort();
    }
    /* See whether it was successful. */
    if (exception_handled)
      exception_handled = 0;
    else {
      /* Call exception handler 2. */
      in_msg.msg_remote_port = old_exception_port;
      in_msg.msg_local_port = main_thread_port;
      retval = msg_send(&in_msg,MSG_OPTION_NONE,0);
      if (!(retval==KERN_SUCCESS)) {
        printf("Mach msg_send to old_exception_port didn't succeed.\n");
        abort();
      }
    }
  }
#undef out_msg
#undef in_msg
}

int sigsegv_install_handler (sigsegv_handler_t handler)
{
  user_handler = handler;
  {
    static int already_installed = 0;
    if (!already_installed) {
      /* Save old exception port. */
      if (!(task_get_exception_port(task_self(),&old_exception_port)==KERN_SUCCESS)) {
        printf("Mach task_get_exception_port fails.\n");
        abort();
      }
      /* Set new exception port. */
      if (!(port_allocate(task_self(),&new_exception_port)==KERN_SUCCESS)) {
        printf("Mach port_allocate fails.\n");
        abort();
      }
      if (!(task_set_exception_port(task_self(),new_exception_port)==KERN_SUCCESS)) {
        printf("Mach task_set_exception_port fails.\n");
        abort();
      }
      /* Create thread for exception handling. */
      cthread_detach(cthread_fork(&exception_thread_main,NULL));
      already_installed = 1;
    }
  }
  return 0;
}

void sigsegv_deinstall_handler (void)
{
  user_handler = &no_user_handler;
  /* The exception thread keeps running, but behaves as if it weren't there. */
}

void sigsegv_leave_handler (void)
{
}

#endif /* NeXTstep */

#if defined(_WIN32) /* Win32 */

#define WIN32_LEAN_AND_MEAN /* avoid including junk */
#include <windows.h>
#include <winerror.h>
/*
 * extern LPTOP_LEVEL_EXCEPTION_FILTER SetUnhandledExceptionFilter (LPTOP_LEVEL_EXCEPTION_FILTER TopLevelExceptionFilter);
 * extern DWORD VirtualQuery (LPCVOID Address, PMEMORY_BASIC_INFORMATION Buffer, DWORD Length);
 * extern BOOL VirtualProtect (LPVOID Address, DWORD Size, DWORD NewProtect, PDWORD OldProtect);
 * extern DWORD GetLastError (void);
 */

/*
 * Stack overflow handling is tricky:
 * First, we must catch a STATUS_STACK_OVERFLOW exception. This is signalled
 * when the guard page at the end of the stack has been touched. The operating
 * system remaps the page with protection PAGE_READWRITE and only then calls
 * our exception handler. Actually, it's even more complicated: The stack has
 * the following layout:
 *
 *         |                             |guard|----------stack-----------|
 *
 * and when the guard page is touched, the system maps it PAGE_READWRITE and
 * allocates a new guard page below it:
 *
 *         |                       |guard|-------------stack--------------|
 *
 * Only when no new guard page can be allocated (because the maximum stack
 * size has been reached), will we see an exception.
 *
 *         |guard|-------------------------stack--------------------------|
 *
 * Second, we must reinstall the guard page. Otherwise, on the next stack
 * overflow, the application will simply crash (on WinNT: silently, on Win95:
 * with an error message box and freezing the system).
 * But since we don't know where %esp points to during the exception handling,
 * we must first leave the exception handler, before we can restore the guard
 * page. And %esp must be made to point to a reasonable value before we do
 * this.
 *
 * Note: On WinNT, the guard page has protection PAGE_READWRITE|PAGE_GUARD.
 * On Win95, which doesn't know PAGE_GUARD, it has protection PAGE_NOACCESS.
 */

static stackoverflow_handler_t stk_user_handler = (stackoverflow_handler_t)NULL;
static unsigned long stk_extra_stack;
static unsigned long stk_extra_stack_size;

static void stack_overflow_handler (unsigned long faulting_page_address)
{
  MEMORY_BASIC_INFORMATION info;
  DWORD oldprot;
  unsigned long base;
  unsigned long address;
  /* First get stack's base address. */
  if (!(VirtualQuery((void*)faulting_page_address,&info,sizeof(info)) == sizeof(info)))
    goto failed;
  base = (unsigned long)info.AllocationBase;
  /* Now search for the first existing page. */
  address = base;
  for (;;) {
    if (!(VirtualQuery((void*)address,&info,sizeof(info)) == sizeof(info)))
      goto failed;
    if (!(address == (unsigned long)info.BaseAddress)) goto failed;
    if (!(info.State == MEM_FREE)) {
      if (!((unsigned long)info.AllocationBase == base)) goto failed;
      if (info.State == MEM_COMMIT) break;
    }
    address = (unsigned long)info.BaseAddress + info.RegionSize;
  }
  /* Now add the PAGE_GUARD bit to the first existing page. */
  /* On WinNT this works... */
  if (VirtualProtect(info.BaseAddress,0x1000,info.Protect|PAGE_GUARD,&oldprot))
    goto ok;
  if (GetLastError() == ERROR_INVALID_PARAMETER)
    /* ... but on Win95 we need this: */
    if (VirtualProtect(info.BaseAddress,0x1000,PAGE_NOACCESS,&oldprot))
      goto ok;
failed:
  for (;;)
    (*stk_user_handler)(1);
ok:
  for (;;)
    (*stk_user_handler)(0);
}

/* This is the stack overflow and page fault handler. */
static LONG WINAPI main_exception_filter (EXCEPTION_POINTERS* ExceptionInfo)
{
  if ((stk_user_handler
       && ExceptionInfo->ExceptionRecord->ExceptionCode == STATUS_STACK_OVERFLOW
      )
      ||
      (user_handler != &no_user_handler
       && ExceptionInfo->ExceptionRecord->ExceptionCode == EXCEPTION_ACCESS_VIOLATION
     )) {
#if 0 /* for debugging only */
    printf("Exception!\n");
    printf("Code = 0x%x\n",
           ExceptionInfo->ExceptionRecord->ExceptionCode);
    printf("Flags = 0x%x\n",
           ExceptionInfo->ExceptionRecord->ExceptionFlags);
    printf("Address = 0x%x\n",
           ExceptionInfo->ExceptionRecord->ExceptionAddress);
    printf("Params:");
    {
      DWORD i;
      for (i = 0; i < ExceptionInfo->ExceptionRecord->NumberParameters; i++)
        printf(" 0x%x,",
               ExceptionInfo->ExceptionRecord->ExceptionInformation[i]);
    }
    printf("\n");
    printf("Registers:\n");
    printf("eip = 0x%x\n", ExceptionInfo->ContextRecord->Eip);
    printf("eax = 0x%x, ", ExceptionInfo->ContextRecord->Eax);
    printf("ebx = 0x%x, ", ExceptionInfo->ContextRecord->Ebx);
    printf("ecx = 0x%x, ", ExceptionInfo->ContextRecord->Ecx);
    printf("edx = 0x%x\n", ExceptionInfo->ContextRecord->Edx);
    printf("esi = 0x%x, ", ExceptionInfo->ContextRecord->Esi);
    printf("edi = 0x%x, ", ExceptionInfo->ContextRecord->Edi);
    printf("ebp = 0x%x, ", ExceptionInfo->ContextRecord->Ebp);
    printf("esp = 0x%x\n", ExceptionInfo->ContextRecord->Esp);
#endif
    if (ExceptionInfo->ExceptionRecord->NumberParameters == 2) {
      if (stk_user_handler
          && ExceptionInfo->ExceptionRecord->ExceptionCode == STATUS_STACK_OVERFLOW) {
        char* address = (char*)(ExceptionInfo->ExceptionRecord->ExceptionInformation[1]);
        /* Restart the program, giving it a sane value for %esp. */
        unsigned long faulting_page_address = (unsigned long)address & -0x1000;
        unsigned long new_safe_esp = ((stk_extra_stack + stk_extra_stack_size) & -8);
        new_safe_esp -= 8; /* make room for argument */
        ExceptionInfo->ContextRecord->Esp = new_safe_esp;
        /* Call stack_overflow_handler(faulting_page_address). */
        ExceptionInfo->ContextRecord->Eip = (unsigned long)&stack_overflow_handler;
        *(unsigned long *)(new_safe_esp + 4) = faulting_page_address;
        return EXCEPTION_CONTINUE_EXECUTION;
      }
      if (user_handler != &no_user_handler
          && ExceptionInfo->ExceptionRecord->ExceptionCode == EXCEPTION_ACCESS_VIOLATION) {
        /* ExceptionInfo->ExceptionRecord->ExceptionInformation[0] is 1 if
         * it's a write access, 0 if it's a read access. But we don't need
         * this info because we don't have it on Unix either.
         */
        void* address = (void*)(ExceptionInfo->ExceptionRecord->ExceptionInformation[1]);
        if ((*user_handler)(address))
          return EXCEPTION_CONTINUE_EXECUTION;
      }
    }
  }
  return EXCEPTION_CONTINUE_SEARCH;
}

static int main_exception_filter_installed = 0;

int sigsegv_install_handler (sigsegv_handler_t handler)
{
  user_handler = handler;
  if (!main_exception_filter_installed) {
    SetUnhandledExceptionFilter((LPTOP_LEVEL_EXCEPTION_FILTER)&main_exception_filter);
    main_exception_filter_installed = 1;
  }
  return 0;
}

void sigsegv_deinstall_handler (void)
{
  user_handler = &no_user_handler;
}

void sigsegv_leave_handler (void)
{
}

void stackoverflow_install_handler (stackoverflow_handler_t handler,
                                    void* extra_stack, unsigned long extra_stack_size)
{
  stk_user_handler = handler;
  stk_extra_stack = (unsigned long) extra_stack;
  stk_extra_stack_size = extra_stack_size;
  if (!main_exception_filter_installed) {
    SetUnhandledExceptionFilter((LPTOP_LEVEL_EXCEPTION_FILTER)&main_exception_filter);
    main_exception_filter_installed = 1;
  }
}

void stackoverflow_deinstall_handler (void)
{
  stk_user_handler = (stackoverflow_handler_t)NULL;
}

#endif /* Win32 */

#else /* no HAVE_SIGSEGV_RECOVERY */

int sigsegv_install_handler (sigsegv_handler_t handler)
{
  return -1;
}

void sigsegv_deinstall_handler (void)
{
}

void sigsegv_leave_handler (void)
{
}

#endif
