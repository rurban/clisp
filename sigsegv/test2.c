/* Test the dispatcher. */

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

static sigsegv_dispatcher dispatcher;

static unsigned int logcount = 0;
static unsigned long logdata[3];

static int area_handler (void* fault_address, void* user_arg)
{
  unsigned long area = *(unsigned long *)user_arg;
  logdata[logcount++] = area;
  if (((unsigned long)fault_address & -0x4000) != area) abort();
  if (mprotect((void*)area,0x4000,PROT_READ_WRITE) == 0) return 1;
  return 0;
}

int handler (void* fault_address, int serious)
{
  return sigsegv_dispatch(&dispatcher,fault_address);
}

void barrier ()
{
}

int main ()
{
  unsigned long area1 = 0x12340000;
  unsigned long area2 = 0x0BEE0000;
  unsigned long area3 = 0x06990000;

#if (defined(HAVE_MMAP_DEVZERO) || defined(HAVE_MMAP_DEVZERO_SUN4_29)) && !defined(HAVE_MMAP_ANON)
  zero_fd = open("/dev/zero",O_RDONLY,0644);
#endif
  sigsegv_init(&dispatcher);
  sigsegv_install_handler(&handler);

#ifdef HAVE_WIN32_VM
  VirtualAlloc((void*)(area1 & -0x10000),0x10000,MEM_RESERVE,PAGE_NOACCESS);
#endif
  mmap_zeromap((void*)area1,0x4000);
  sigsegv_register(&dispatcher,(void*)area1,0x4000,&area_handler,&area1);
  mprotect((void*)area1,0x4000,PROT_NONE);

#ifdef HAVE_WIN32_VM
  VirtualAlloc((void*)(area2 & -0x10000),0x10000,MEM_RESERVE,PAGE_NOACCESS);
#endif
  mmap_zeromap((void*)area2,0x4000);
  sigsegv_register(&dispatcher,(void*)area2,0x4000,&area_handler,&area2);
  mprotect((void*)area2,0x4000,PROT_READ);

#ifdef HAVE_WIN32_VM
  VirtualAlloc((void*)(area3 & -0x10000),0x10000,MEM_RESERVE,PAGE_NOACCESS);
#endif
  mmap_zeromap((void*)area3,0x4000);
  sigsegv_register(&dispatcher,(void*)area3,0x4000,&area_handler,&area3);
  mprotect((void*)area3,0x4000,PROT_READ);

  ((int*)area2)[230] = 22;
  ((int*)area3)[412] = 33;
  ((int*)area2)[135] = 22;
  ((int*)area1)[612] = 11;
  barrier();
  if (logcount != 3) exit(1);
  if (!(logdata[0] == area2 && logdata[1] == area3 && logdata[2] == area1))
    exit(1);
  return 0;
}

#else

int main ()
{
  return 0;
}

#endif
