/* This file is derived from the GCC sources.  */

/* Copyright (C) 1989-2006 Free Software Foundation, Inc.

This file is part of GNU CC.

GNU CC is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2, or (at your option)
any later version.

GNU CC is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GNU CC; see the file COPYING.  If not, write to
the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.  */


/* Taken from gcc-4.1.0/gcc/config/alpha/osf.h.  */

#if defined (__alpha__)

void
__enable_execute_stack (void *addr)
{
  long size = getpagesize ();
  long mask = ~(size-1);
  char *page = (char *) (((long) addr) & mask);
  char *end  = (char *) ((((long) (addr + TRAMPOLINE_SIZE)) & mask) + size);

  /* 7 is PROT_READ | PROT_WRITE | PROT_EXEC */
  if (mprotect (page, end - page, 7) < 0)
    perror ("mprotect of trampoline code");
}

#endif

/* Taken from gcc-4.1.0/gcc/config/sparc/freebsd.h.  */

#if defined (__sparc__)

extern int sysctlbyname(const char *, void *, size_t *, void *, size_t);
static int need_enable_exec_stack;
static void check_enabling(void) __attribute__ ((constructor));
static void check_enabling(void)
{
  int prot = 0;
  size_t len = sizeof(prot);

  sysctlbyname ("kern.stackprot", &prot, &len, NULL, 0);
  if (prot != 7)
    need_enable_exec_stack = 1;
}
void __enable_execute_stack (void *addr)
{
  if (!need_enable_exec_stack)
    return;
  else {
    /* 7 is PROT_READ | PROT_WRITE | PROT_EXEC */
    if (mprotect (addr, TRAMPOLINE_SIZE, 7) < 0)
      perror ("mprotect of trampoline code");
  }
}

#endif

/* Taken from gcc-4.1.0/gcc/config/sol2.h.  */

#if (defined (__sparc__) || defined (__i386__)) && defined(__svr4__) && defined(__sun)

extern long sysconf(int);

static int need_enable_exec_stack;

static void check_enabling(void) __attribute__ ((constructor));
static void check_enabling(void)
{
  int prot = (int) sysconf(515 /* _SC_STACK_PROT */);
  if (prot != 7 /* STACK_PROT_RWX */)
    need_enable_exec_stack = 1;
}

void
__enable_execute_stack (void *addr)
{
  if (!need_enable_exec_stack)
    return;
  else {
    long size = getpagesize ();
    long mask = ~(size-1);
    char *page = (char *) (((long) addr) & mask);
    char *end  = (char *) ((((long) (addr + TRAMPOLINE_SIZE)) & mask) + size);

    if (mprotect (page, end - page, 7 /* STACK_PROT_RWX */) < 0)
      perror ("mprotect of trampoline code");
  }
}

#endif

/* Taken from gcc-4.1.0/gcc/config/openbsd.h.  */

#if defined (__OpenBSD__)

/* Stack is explicitly denied execution rights on OpenBSD platforms.  */
void
__enable_execute_stack (void *addr)
{
  long size = getpagesize ();
  long mask = ~(size-1);
  char *page = (char *) (((long) addr) & mask);
  char *end  = (char *) ((((long) (addr + TRAMPOLINE_SIZE)) & mask) + size);

  if (mprotect (page, end - page, PROT_READ | PROT_WRITE | PROT_EXEC) < 0)
    perror ("mprotect of trampoline code");
}

#endif

/* Taken from gcc-4.1.0/gcc/config/netbsd.h.  */

#if defined (__NetBSD__)

extern int __sysctl (int *, unsigned int, void *, size_t *, void *, size_t);

void
__enable_execute_stack (void *addr)
{
  static int size;
  static long mask;

  char *page, *end;

  if (size == 0)
    {
      int mib[2];
      size_t len;

      mib[0] = 6; /* CTL_HW */
      mib[1] = 7; /* HW_PAGESIZE */
      len = sizeof (size);
      (void) __sysctl (mib, 2, &size, &len, NULL, 0);
      mask = ~((long) size - 1);
    }

  page = (char *) (((long) addr) & mask);
  end  = (char *) ((((long) (addr + TRAMPOLINE_SIZE)) & mask) + size);

  /* 7 == PROT_READ | PROT_WRITE | PROT_EXEC */
  (void) mprotect (page, end - page, 7);
}

#endif

/* The rest is taken from gcc-2.6.3/libgcc2.c.  */

#if defined (NeXT) && defined (__MACH__)

/* Make stack executable so we can call trampolines on stack.
   This is called from INITIALIZE_TRAMPOLINE in next.h.  */
#ifdef NeXTStep21
 #include <mach.h>
#else
 #include <mach/mach.h>
#endif

void
__enable_execute_stack (addr)
     char *addr;
{
  kern_return_t r;
  char *eaddr = addr + TRAMPOLINE_SIZE;
  vm_address_t a = (vm_address_t) addr;

  /* turn on execute access on stack */
  r = vm_protect (task_self (), a, TRAMPOLINE_SIZE, FALSE, VM_PROT_ALL);
  if (r != KERN_SUCCESS)
    {
      mach_error("vm_protect VM_PROT_ALL", r);
      exit(1);
    }
} 

#endif /* defined (NeXT) && defined (__MACH__) */

#ifdef __convex__

/* Make stack executable so we can call trampolines on stack.
   This is called from INITIALIZE_TRAMPOLINE in convex.h.  */

#include <sys/mman.h>
#include <sys/vmparam.h>
#include <machine/machparam.h>

void
__enable_execute_stack ()
{
  int fp;
  static unsigned lowest = USRSTACK;
  unsigned current = (unsigned) &fp & -NBPG;

  if (lowest > current)
    {
      unsigned len = lowest - current;
      mremap (current, &len, PROT_READ | PROT_WRITE | PROT_EXEC, MAP_PRIVATE);
      lowest = current;
    }
}
#endif /* __convex__ */

#ifdef __DOLPHIN__

/* Modified from the convex -code above. */

#include <sys/param.h>
#include <errno.h>
#include <sys/m88kbcs.h>

void
__enable_execute_stack ()
{
  int save_errno;
  static unsigned long lowest = USRSTACK;
  unsigned long current = (unsigned long) &save_errno & -NBPC;
  
  /* Ignore errno being set. memctl sets errno to EINVAL whenever the
     address is seen as 'negative'. That is the case with the stack.   */

  save_errno=errno;
  if (lowest > current)
    {
      unsigned len=lowest-current;
      memctl(current,len,MCT_TEXT);
      lowest = current;
    }
  else
    memctl(current,NBPC,MCT_TEXT);
  errno=save_errno;
}

#endif /* __DOLPHIN__ */

#ifdef __pyr__

#undef NULL /* Avoid errors if stdio.h and our stddef.h mismatch.  */
#include <stdio.h>
#include <sys/mman.h>
#include <sys/types.h>
#include <sys/param.h>
#include <sys/vmmac.h>

/* Modified from the convex -code above.
   mremap promises to clear the i-cache. */

void
__enable_execute_stack ()
{
  int fp;
  if (mprotect (((unsigned int)&fp/PAGSIZ)*PAGSIZ, PAGSIZ,
		PROT_READ|PROT_WRITE|PROT_EXEC))
    {
      perror ("mprotect in __enable_execute_stack");
      fflush (stderr);
      abort ();
    }
}
#endif /* __pyr__ */
