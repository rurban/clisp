/* Test that the handler is called, with the right fault address. */

#include "sigsegv.h"

#ifdef _WIN32
#define HAVE_WIN32_VM
#else
#include "config.h"
#endif

#if defined(HAVE_SIGSEGV_RECOVERY) && (((defined(HAVE_MMAP_ANONYMOUS) || defined(HAVE_MMAP_ANON) || defined(HAVE_MMAP_DEVZERO) || defined(HAVE_MMAP_DEVZERO_SUN4_29)) && defined(HAVE_WORKING_MPROTECT)) || defined(HAVE_MACH_VM) || defined(HAVE_WIN32_VM))

/* First some auxiliary stuff for using mmap & friends. */

#if defined(HAVE_MACH_VM)

#include <sys/resource.h>
#include <mach/mach_interface.h>
#ifdef NeXT
#include <mach/mach_init.h>
#endif
#ifdef __osf__
#include <mach_init.h>
#endif
#include <mach/mach_traps.h>
#include <mach/machine/vm_param.h>
#define PROT_NONE  0
#define PROT_READ  VM_PROT_READ
#define PROT_WRITE VM_PROT_WRITE
#define PROT_EXEC  VM_PROT_EXECUTE
#define PROT_READ_WRITE  (PROT_READ|PROT_WRITE)

static int mmap_zeromap (void* map_addr, vm_size_t map_len)
{
  if (vm_allocate(task_self(), (vm_address_t*) &map_addr, map_len, 0) == KERN_SUCCESS)
    return 0;
  else
    return -1;
}

int munmap (vm_address_t addr, vm_size_t len)
{
  if (vm_deallocate(task_self(),addr,len) == KERN_SUCCESS)
    return 0;
  else
    return -1;
}

int mprotect (vm_address_t addr, vm_size_t len, int prot)
{
  if (vm_protect(task_self(),addr,len,0,prot) == KERN_SUCCESS)
    return 0;
  else
    return -1;
}

#endif

#if defined(HAVE_WIN32_VM)

#define WIN32_LEAN_AND_MEAN /* avoid including junk */
#include <windows.h>
#include <winerror.h>
#define PROT_NONE  PAGE_NOACCESS
#define PROT_READ  PAGE_READONLY
#define PROT_READ_WRITE PAGE_READWRITE

static int mmap_zeromap (void* map_addr, unsigned long map_len)
{
  if (VirtualAlloc(map_addr,map_len,MEM_COMMIT,PAGE_READWRITE))
    return 0;
  else
    return -1;
}

int munmap (void* addr, unsigned long len)
{
  if (VirtualFree(addr,len,MEM_DECOMMIT))
    return 0;
  else
    return -1;
}

int mprotect (void* addr, unsigned long len, int prot)
{
  DWORD oldprot;
  if (VirtualProtect(addr,len,prot,&oldprot))
    return 0;
  else
    return -1;
}

#endif

#if defined(HAVE_MMAP_ANONYMOUS) || defined(HAVE_MMAP_ANON) || defined(HAVE_MMAP_DEVZERO) || defined(HAVE_MMAP_DEVZERO_SUN4_29)

#include <sys/types.h>
#include <sys/mman.h>

#ifndef PROT_NONE
#define PROT_NONE 0
#endif
#define PROT_READ_WRITE  (PROT_READ|PROT_WRITE)

#ifdef HAVE_MMAP_ANONYMOUS
#define zero_fd  -1
#define map_flags  MAP_ANONYMOUS | MAP_PRIVATE
#else
#ifdef HAVE_MMAP_ANON
#define zero_fd  -1
#define map_flags  MAP_ANON | MAP_PRIVATE
#else
#include <fcntl.h>
#ifdef OPEN_NEEDS_SYS_FILE_H
#include <sys/file.h>
#endif
static int zero_fd;
#ifdef MAP_FILE
#define map_flags  MAP_FILE | MAP_PRIVATE
#else
#define map_flags  MAP_PRIVATE
#endif
#endif
#endif

static int mmap_zeromap (void* map_addr, unsigned long map_len)
{
  if ((void*) mmap(map_addr,map_len, PROT_READ_WRITE, map_flags | MAP_FIXED, zero_fd, 0) == (void*)(-1))
    return -1;
  else
    return 0;
}

#endif

/* Now the test program. */

int handler_called = 0;

int handler (void* fault_address, int serious)
{
  handler_called++;
  if (fault_address != (void*)0x12340678) abort();
  if (mprotect((void*)((unsigned long)fault_address & -0x4000),0x4000,PROT_READ_WRITE) == 0) return 1;
  return 0;
}

void crasher (unsigned long p)
{
  *(int*)(p + 0x678) = 42;
}

int main ()
{
  unsigned long page = 0x12340000;

#if (defined(HAVE_MMAP_DEVZERO) || defined(HAVE_MMAP_DEVZERO_SUN4_29)) && !defined(HAVE_MMAP_ANON)
  zero_fd = open("/dev/zero",O_RDONLY,0644);
#endif
#ifdef HAVE_WIN32_VM
  VirtualAlloc((void*)(page & -0x10000),0x10000,MEM_RESERVE,PAGE_NOACCESS);
#endif
  mmap_zeromap((void*)page,0x4000);
  mprotect((void*)page,0x4000,PROT_READ);
  sigsegv_install_handler(&handler);
  crasher(page);
  crasher(page);
  if (handler_called != 1) exit(1);
  return 0;
}

#else

int main ()
{
  return 0;
}

#endif
