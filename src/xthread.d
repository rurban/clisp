/*
 * Cross-platform thread support
 * Bruno Haible 1997-1999
 * Sam Steingold 2001-2008

 =============================================================================
 This part is heavily influenced by the file <X11/Xthreads.h> from X11R6,
 which carries the following copyright:
 -----------------------------------------------------------------------------
 Copyright (c) 1993  X Consortium

 Permission is hereby granted, free of charge, to any person obtaining a copy
 of this software and associated documentation files (the "Software"), to deal
 in the Software without restriction, including without limitation the rights
 to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
 copies of the Software, and to permit persons to whom the Software is
 furnished to do so, subject to the following conditions:

 The above copyright notice and this permission notice shall be included in
 all copies or substantial portions of the Software.

 THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.  IN NO EVENT SHALL THE
 X CONSORTIUM BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN
 AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
 CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

 Except as contained in this notice, the name of the X Consortium shall not be
 used in advertising or otherwise to promote the sale, use or other dealings
 in this Software without prior written authorization from the X Consortium.
 -----------------------------------------------------------------------------

 Upon entry to this file, one of these symbols shall be defined:
 POSIX_THREADS       POSIX.1c            pthread_*
 SOLARIS_THREADS     Solaris 2.4, 2.5    thr_*
 WIN32_THREADS       Win32               *Thread

 This file defines the following types:
   xthread_t         Type of a thread
   xcondition_t      Type of a condition (wait queue with signalling)
   xmutex_t          Type of a mutex (mutually exclusive lock)
   xthread_key_t     Type of a key for accessing (limited) thread-local storage
 and the following functions/macros:
 - Threads in general:
   - Initialization of the thread subsystem.
   extern void           xthread_init (void);
   - Return the current thread.
   extern xthread_t      xthread_self (void);
   - Create a new thread.
   extern int            xthread_create (xthread_t* thread, void* (*startroutine) (void*), void* arg);
   - Terminate the current thread.
   extern void           xthread_exit (void* retvalue);
   - Give other threads a chance to run.
   extern void           xthread_yield (void);
   - Compare two threads for identity.
   extern bool        xthread_equal (xthread_t thread1, xthread_t thread2);
 - Conditions:
   - Initialize a wait queue.
   extern int            xcondition_init (xcondition_t* c);
   - Destroy a wait queue. The wait queue must be empty (noone waiting).
   extern int            xcondition_destroy (xcondition_t* c);
   - Release a mutex and put the current thread into the wait queue.
   - When the wait ends, the mutex is acquired again.
   extern int            xcondition_wait (xcondition_t* c, xmutex_t* m);
   - Notify and unblock one thread in the wait queue.
   extern int            xcondition_signal (xcondition_t* c);
   - Notify and unblock all threads in the wait queue.
   extern int            xcondition_broadcast (xcondition_t* c);
 - Mutexes:
   - Initialize a mutex.
   extern int            xmutex_init (xmutex_t* m);
   - Destroy a mutex.
   extern int            xmutex_destroy (xmutex_t* m);
   - Lock a mutex.
   extern int            xmutex_lock (xmutex_t* m);
   - Unlock a mutex.
   extern int            xmutex_unlock (xmutex_t* m);
 - Thread-local storage:
   - (This is probably not useful at all. The number of thread-local storage
   - words is limited: 512 on Win32, 128 with LinuxThreads. And it's probably
   - much slower than my current_thread() function.)
   - Create a word of thread-local storage, and return a key to it.
   extern int            xthread_key_create (xthread_key_t* key);
   - Delete a word of thread-local storage.
   extern int            xthread_key_delete (xthread_key_t key);
   - Get the value of the thread-local storage word for the current thread.
   extern void*          xthread_key_get (xthread_key_t key);
   - Set the value of the thread-local storage word for the current thread.
   extern void           xthread_key_set (xthread_key_t key, void* value);
*/


#if !(defined(POSIX_THREADS) || defined(SOLARIS_THREADS) || defined(WIN32_THREADS))
  #error Define your flavour of multithreading
#endif

/* NOTE: This file is not yet finished. The primary target is POSIX_THREADS.
 For the other targets, the error checking needs to be improved.

 NOTE 2: Some of the macros in this file require gcc. */

#if defined(POSIX_THREADS)

/* The default pthreads mutex is not recursive. This is not a problem however
 the Win32 critical section (used for mutex) is recursive and there is no way
 to disable this behavior. In order the xmutex_t to be cosistent we wrap the
 default pthread_mutex_t in a "recursive" shell.
 (we can use later on pthread mutex attributes for this where available) */

#include <pthread.h>
#include <sched.h>

typedef pthread_t         xthread_t;
typedef pthread_cond_t    xcondition_t;
/* on some platforms PTHREAD_MUTEXT_RECURSIVE_NP is not macro but in an enum */
#if defined(PTHREAD_MUTEX_RECURSIVE_NP) || defined(PTHREAD_RECURSIVE_MUTEX_INITIALIZER_NP)
  typedef pthread_mutex_t xmutex_t;
  /* cache the global mutex attribute for recursive mutex creation */
  extern pthread_mutexattr_t recursive_mutexattr;
  #define xthread_init() \
    do {                 \
      pthread_mutexattr_init(&recursive_mutexattr);\
      pthread_mutexattr_settype(&recursive_mutexattr,PTHREAD_MUTEX_RECURSIVE_NP);\
    } while (0)
#else
  typedef struct xmutex_t {
    pthread_mutex_t cs;
    int count;
    xthread_t owner;
  } xmutex_t;
  #define xthread_init()
#endif
typedef pthread_key_t     xthread_key_t;

#define xthread_self()  pthread_self()
static inline int xthread_create(xthread_t *thread,void *(*startroutine)(void *),
				 void *arg, size_t stacksize)
{
  int r;
  pthread_attr_t attr;
  pthread_attr_init(&attr);
  if (stacksize)
    pthread_attr_setstacksize(&attr,stacksize);
  pthread_attr_setdetachstate(&attr,PTHREAD_CREATE_DETACHED);
  r=pthread_create(thread,&attr,startroutine,arg);
  pthread_attr_destroy(&attr);
  return r;
}
#define xthread_exit(v)  pthread_exit(v)
#define xthread_yield()  do { if (sched_yield() < 0) OS_error(); } while(0)
#define xthread_equal(t1,t2)  pthread_equal(t1,t2)
#define xthread_signal(t,sig) pthread_kill(t,sig)
#define xthread_sigmask(how,iset,oset) pthread_sigmask(how,iset,oset)

#define xcondition_init(c)  pthread_cond_init(c,NULL)
#define xcondition_destroy(c)  pthread_cond_destroy(c)
#if defined(PTHREAD_MUTEX_RECURSIVE_NP) || defined(PTHREAD_RECURSIVE_MUTEX_INITIALIZER_NP)
 #define xcondition_wait(c,m)  pthread_cond_wait(c,m)
#else
 #define xcondition_wait(c,m)  pthread_cond_wait(c,&(m)->cs)
#endif
#define xcondition_timedwait(c,m,to)  pthread_cond_timedwait(c,m,to)
#define xcondition_signal(c)  pthread_cond_signal(c)
#define xcondition_broadcast(c)  pthread_cond_broadcast(c)

#if defined(PTHREAD_MUTEX_RECURSIVE_NP) || defined(PTHREAD_RECURSIVE_MUTEX_INITIALIZER_NP)
 #define xmutex_init(m) pthread_mutex_init(m,&recursive_mutexattr)
 #define xmutex_destroy(m)  pthread_mutex_destroy(m)
 #define xmutex_lock(m)  pthread_mutex_lock(m)
 #define xmutex_trylock(m) (pthread_mutex_trylock(m)==0)
 #define xmutex_unlock(m)  pthread_mutex_unlock(m)
#else /* no recursive pthread mutex - implement one */
 #define xmutex_init(m)                                                  \
  ((m)->count=0,(m)->owner=0,pthread_mutex_init(&(m)->cs,NULL))
 #define xmutex_destroy(m)  pthread_mutex_destroy(&(m)->cs)
 #define xmutex_lock(m)  \
  do {\
    xthread_t self=xthread_self();\
    if ((m)->owner != self) { pthread_mutex_lock(&(m)->cs); (m)->owner = self; } \
    (m)->count++;                                                       \
  } while(0)
 #define xmutex_trylock(m) \
  (xthread_self()==(m)->owner ? ++(m)->count :                          \
   ((pthread_mutex_trylock(&(m)->cs)==0) ? (m)->owner=xthread_self(), ++(m)->count : 0))
 #define xmutex_unlock(m)  \
  if (--(m)->count == 0) { (m)->owner = 0; pthread_mutex_unlock(&(m)->cs); }
#endif

#define xthread_key_create(key)  pthread_key_create(key,NULL)
#define xthread_key_delete(key)  pthread_key_delete(key)
#define xthread_key_get(key)  pthread_getspecific(key)
#define xthread_key_set(key,val)  pthread_setspecific(key,val)

#endif  /* POSIX*_THREADS */

#if defined(SOLARIS_THREADS)

#include <thread.h>
#include <synch.h>

typedef thread_t          xthread_t;
typedef cond_t            xcondition_t;
typedef mutex_t           xmutex_t;
typedef thread_key_t      xthread_key_t;

#define xthread_init()
#define xthread_self()  thr_self()
#define xthread_create(thread,startroutine,arg,stacksize) \
  thr_create(NULL,stacksize,startroutine,arg,THR_NEW_LWP|THR_DETACHED,thread)
#define xthread_exit(v)  thr_exit(v)
#define xthread_yield()  thr_yield()
#define xthread_equal(t1,t2)  ((t1)==(t2))
#define xthread_signal(t,sig) thr_kill(t,sig)

#define xcondition_init(c)  cond_init(c,USYNC_THREAD,0)
#define xcondition_destroy(c)  cond_destroy(c)
#define xcondition_wait(c,m)  cond_wait(c,m)
#define xcondition_signal(c)  cond_signal(c)
#define xcondition_broadcast(c)  cond_broadcast(c)

#define xmutex_init(m)  mutex_init(m,USYNC_THREAD|LOCK_RECURSIVE|LOCK_ERRORCHECK,0)
#define xmutex_destroy(m)  mutex_destroy(m)
#define xmutex_trylock(m) mutex_trylock(m)
#define xmutex_lock(m)  mutex_lock(m)
#define xmutex_unlock(m)  mutex_unlock(m)


#define xthread_key_create(key)  thr_keycreate(key,NULL)
#define xthread_key_delete(key)  0
/* on Solaris - Sun and GNU compilers support "statement expression" */
#define xthread_key_get(key)  \
  ({ void* _tmp; thr_getspecific(key,&_tmp); _tmp; })
#define xthread_key_set(key,val)  thr_setspecific(key,val)
#define xthread_sigmask(how,iset,oset) thr_sigmask(how,iset,oset)

#endif  /* SOLARIS_THREADS */

#if defined(WIN32_THREADS)

/* include <windows.h>  -- already included by win32.d */
#define MAX_SEMAPHORE_COUNT  128

typedef DWORD              xthread_t;
struct _xcondition {
  CRITICAL_SECTION cs;
  HANDLE sem;
  int waiting_count;
};
typedef struct _xcondition xcondition_t;
typedef CRITICAL_SECTION   xmutex_t;
typedef DWORD              xthread_key_t;

#define xthread_init()
#define xthread_self()  GetCurrentThreadId()
/* xthread_create() should return 0 on success */
#define xthread_create(thread,startroutine,arg,stacksize)                       \
  (!CreateThread(NULL,stacksize,(LPTHREAD_START_ROUTINE)startroutine,(LPVOID)arg,0,thread))
#define xthread_exit(v)  ExitThread((DWORD)(v))
#define xthread_yield()  Sleep(0)
#define xthread_equal(t1,t2)  ((t1)==(t2))

#define xcondition_init(c)						\
  (InitializeCriticalSection(&(c)->cs),                                 \
   (c)->sem=CreateSemaphore(NULL,0,MAX_SEMAPHORE_COUNT,NULL),		\
   (c)->waiting_count=0,						\
   GetLastError())
#define xcondition_destroy(c)  do {             \
  DeleteCriticalSection(&(c)->cs);              \
  CloseHandle((c)->sem);                        \
 } while (0)

#define xcondition_wait(c,m)  do {                                  \
  EnterCriticalSection(&(c)->cs);                                   \
  (c)->waiting_count++;                                             \
  LeaveCriticalSection(&(c)->cs);                                   \
  LeaveCriticalSection(m);                                          \
  WaitForSingleObject((c)->sem,INFINITE);                           \
  EnterCriticalSection(m);                                          \
 } while(0)
#define xcondition_signal(c)  do {                                     \
  EnterCriticalSection(&(c)->cs);                                      \
  if ((c)->waiting_count > 0) {                                        \
    ReleaseSemaphore((c)->sem,1,NULL);                                 \
    (c)->waiting_count--;                                              \
  }                                                                    \
  LeaveCriticalSection(&(c)->cs);                                      \
 } while(0)
#define xcondition_broadcast(c)  do {                                   \
  EnterCriticalSection(&(c)->cs);                                       \
  if ((c)->waiting_count > 0) {                                         \
    ReleaseSemaphore((c)->sem,(c)->waiting_count,NULL);                 \
    (c)->waiting_count=0;                                               \
  }                                                                     \
  LeaveCriticalSection(&(c)->cs);                                       \
 } while(0)

/* critical section functions do not return values and do not set
   last error */
#define xmutex_init(m) (InitializeCriticalSection(m),0)
#define xmutex_destroy(m)  (DeleteCriticalSection(m),0)
#define xmutex_lock(m)      (EnterCriticalSection(m),0)
#define xmutex_trylock(m) (TryEnterCriticalSection(m)!=0)
#define xmutex_unlock(m)    (LeaveCriticalSection(m),0)

#define xthread_key_create(key)  (*(key) = TlsAlloc())
#define xthread_key_delete(key)  TlsFree(key)
#define xthread_key_get(key)  TlsGetValue(key)
#define xthread_key_set(key,val)  TlsSetValue(key,val)

#endif  /* WIN32_THREADS */

/* ==========================================================================

 Spin-locks.
 This is the most elementary kind of locks.
 Acquiring and releasing of a spin-lock can be considered an atomic operation.
 Differences between spin-locks and mutexes:
 - Spin-locks have to be locked only for a short time; no blocking system
   calls must be performed with a spin-lock held; no other locks can be
   acquired while a spin-lock is held without special care.
 - Therefore spin-locks can be assumed to be unlocked "soon", without any
   particular action to be performed. When trying to acquire a spin-lock which
   is currently locked, all you can do is sit down and spin ("Däumchen drehen"
   in German).
 - Acquiring a lock which is previously unlocked, and releasing a lock are
   fast operations. */


#ifndef TARGET_CPU_DEFINED
#if defined(__vax__)
  #define VAX
#endif
#if defined(arm) || defined(__arm) || defined(__arm__)
  #define ARM
#endif
#if (defined(_WIN32))
  #if defined(_M_IX86) || defined(_X86_)
    #define I80386
  #endif
#else /* some unix flavour */
  #if defined(m68k) || defined(__m68k__) || defined(mc68000)
    #define MC680X0
  #endif
  #if defined(mc68020) || defined(__mc68020__) || (defined(m68k) && defined(NeXT))
    #define MC680X0
    #define MC680Y0
  #endif
  #if defined(i386) || defined(__i386) || defined(__i386__) || defined(_I386)
    #define I80386
  #endif
  #if defined(sparc) || defined(__sparc__)
    #define SPARC
    #if defined(__sparcv9) || defined(__arch64__)
      #define SPARC64
    #endif
  #endif
  #if defined(mips) || defined(__mips) || defined(__mips__)
    #define MIPS
    #if defined(_MIPS_SZLONG)
      #if (_MIPS_SZLONG == 64)
        /* We should also check for (_MIPS_SZPTR == 64), but gcc keeps this at 32. */
        #define MIPS64
      #endif
    #endif
  #endif
  #if defined(HP8XX) || defined(hppa) || defined(__hppa) || defined(__hppa__)
    #define HPPA
  #endif
  #if defined(m88000) || defined(__m88k__)
    #define M88000
  #endif
  #if defined(_IBMR2) || defined(__powerpc) || defined(__ppc) || defined(__ppc__) || defined(__powerpc__)
    #define POWERPC
  #endif
  #ifdef __alpha
    #define DECALPHA
  #endif
  #ifdef __ia64__
    #define IA64
  #endif
  #if defined(__x86_64__) || defined(__amd64__)
    #define AMD64
  #endif
  #ifdef __s390__
    #define S390
  #endif
#endif
#define TARGET_CPU_DEFINED
#endif


#if (defined(MC680X0) || defined(SPARC) || defined(MIPS) || defined(I80386) || defined(DECALPHA) || defined(POWERPC) || defined(AMD64))

  typedef int spinlock_t; /* A value 0 means unlocked, != 0 means locked. */

  /* The following atomic operations are borrowed from LinuxThreads-0.6
   and were mostly written by Richard Henderson <rth@tamu.edu>.

   testandset(spinlock) tries to acquire the spinlock. It returns
   0 if it succeeded (i.e. the old value was 0, the new one is != 0).
   It returns != 0 if it failed (i.e. the old value was != 0). */

  static inline void spinlock_init (int* spinlock)
  { *spinlock = 0; }
  #ifdef MC680X0
    static inline int testandset (int* spinlock)
    { char ret;
      __asm__ __volatile__("tas %1; sne %0"
                           : "=g" (ret), "=m" (*spinlock)
                           : "1" (*spinlock)
                           : "cc"
                          );
      return ret;
    }
    static inline void spinlock_release (int* spinlock)
    { *spinlock = 0; }
  #endif
  #ifdef SPARC
    static inline int testandset (int* spinlock)
    { int ret;
      __asm__ __volatile__("ldstub %1,%0"
                           : "=r" (ret), "=m" (*spinlock)
                           : "1" (*spinlock)
                          );
      return ret;
    }
    static inline void spinlock_release (int* spinlock)
    { __asm__ __volatile__("stbar; stb %1,%0"
                           : "=m" (*spinlock)
                           : "r" (0)
                          );
    }
  #endif
  #ifdef MIPS
    static inline long testandset (int* spinlock)
    { long ret;
      long temp;
      __asm__ __volatile__("#Inline spinlock test & set"
                   "\n\t"  ".set mips2"
                   "\n" "1: ll %0,%2"
                   "\n\t"  "bnez %0,2f"
                   "\n\t"  ".set noreorder"
                   "\n\t"  "li %1,1"
                   "\n\t"  ".set reorder"
                   "\n\t"  "sc %1,%2"
                   "\n\t"  "beqz %1,1b"
                   "\n" "2: .set mips0"
                   "\n\t"  "#End spinlock test & set"
                           : "=&r" (ret), "=&r" (temp), "=m" (*spinlock)
                           : "2" (*spinlock)
                           : "memory"
                          );
      return ret;
    }
    static inline void spinlock_release (int* spinlock)
    { *spinlock = 0; }
  #endif
  #if defined(I80386) || defined(AMD64)
    static inline long testandset (int* spinlock)
    { int ret;
      __asm__ __volatile__("xchgl %0,%1"
                           : "=r" (ret), "=m" (*spinlock)
                           : "0" (1), "m" (*spinlock)
                           : "memory"
                          );
      return ret;
    }
    static inline void spinlock_release (int* spinlock)
    { *spinlock = 0; }
  #endif
  #ifdef POWERPC
    static inline long testandset (int* spinlock)
    { int ret;
      __asm__ __volatile__(
                           "0:lwarx %0,0,%1 \n"
                           " cmpwi %0,0 \n"
                           " bne- 1f \n"
                           " stwcx. %2,0,%1 \n"
                           " bne- 0b \n"
                           "1:  isync  \n"
                           : "=&r"(ret)
                           : "r"(spinlock), "r"(1)
                           : "cr0", "memory");
      return ret;
    }
    static inline void spinlock_release (int* spinlock)
    {
      __asm__ __volatile__("sync" : : : "memory");
      *spinlock = 0;
    }
  #endif
  #ifdef DECALPHA
    static inline long testandset (int* spinlock)
    { long ret;
      long temp;
      __asm__ __volatile__("/* Inline spinlock test & set */"
                   "\n" "1: ldl_l %0,%2"
                   "\n\t"  "bne %0,2f"
                   "\n\t"  "or $31,1,%1"
                   "\n\t"  "stl_c %1,%2"
                   "\n\t"  "beq %1,1b"
                   "\n" "2: mb"
                   "\n\t"  "/* End spinlock test & set */"
                           : "=&r" (ret), "=&r" (temp), "=m" (*spinlock)
                           : "2" (*spinlock)
                           : "memory"
                          );
      return ret;
    }
    static inline void spinlock_release (int* spinlock)
    { __asm__ __volatile__("mb" : : : "memory"); *spinlock = 0; }
  #endif

  static inline bool spinlock_tryacquire(int* spinlock)
  { return testandset(spinlock)==0;}
  static inline void spinlock_acquire (int* spinlock)
  { while (testandset(spinlock)) { xthread_yield(); } }
  static inline void spinlock_destroy (int* spinlock)
  { unused spinlock; }

#elif defined(GNU) && defined(HPPA)

  /* This is borrowed from glibc-2.0.4. */

  typedef int spinlock_t __attribute__((__aligned__(16)));
  /* A value -1 means unlocked, 0 means locked. */

  /* testandset(spinlock) tries to acquire the spinlock. It returns
   0 if it succeeded (i.e. the old value was -1, the new one is 0).
   It returns != 0 if it failed (i.e. the old value was 0). */

  static inline void spinlock_init (int* spinlock)
  { *spinlock = -1; }
  static inline int testandset (int* spinlock)
  { int ret;
    __asm__ __volatile__("ldcws %0,%1"
                         : "=m" (*spinlock), "=r" (ret)
                         : "m" (*spinlock)
                        );
    return ret;
  }
  static inline bool spinlock_tryacquire(int* spinlock)
  { return testandset(spinlock)==0;}
  static inline void spinlock_acquire (int* spinlock)
  { while (testandset(spinlock)) { xthread_yield(); } }
  static inline void spinlock_release (int* spinlock)
  { *spinlock = -1; }
  static inline void spinlock_destroy (int* spinlock)
  { unused spinlock; }

#else

  /* Slow, but portable. */

  typedef xmutex_t spinlock_t;

  /* do not check for errors from xmutex_xxxx() even if there is
     problem it is too generic in order to be handled properly.*/
  static inline void spinlock_init (spinlock_t* spinlock)
  { xmutex_init(spinlock); }
  static inline bool spinlock_tryacquire(spinlock_t* spinlock)
  { return xmutex_trylock(spinlock); }
  static inline void spinlock_acquire (spinlock_t* spinlock)
  { xmutex_lock(spinlock); }
  static inline void spinlock_release (spinlock_t* spinlock)
  { xmutex_unlock(spinlock); }
  static inline void spinlock_destroy (spinlock_t* spinlock)
  { xmutex_destroy(spinlock); }

#endif


/* ====================================================================== */

