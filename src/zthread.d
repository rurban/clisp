/*
 * CLISP thread functions - multiprocessing
 * Distributed under the GNU GPL as a part of GNU CLISP
 * Sam Steingold 2003-2008
 */

#include "lispbibl.c"

#ifdef MULTITHREAD

LISPFUN(make_thread,seclass_default,1,0,norest,key,1,(kw(name)))
{ /* (MAKE-THREAD function &key name) */
  NOTREACHED;
}

struct call_data_t {
  gcv_object_t *caller; /* points to STACK */
  uintC argcount; /* if this is non-0, the args should be on STACK */
  xmutex_t mutex;
  xcondition_t cond;
  bool done;
  clisp_thread_t *calling_thread;
};

/* execute the call specified by the call_data_t argument
 can trigger GC */
local maygc void *exec_call (void *arg)
{
  struct call_data_t *pcd = (struct call_data_t*)arg;
  uintC argcount = pcd->argcount;
  xmutex_lock(&pcd->mutex); /* wait for the main thread to start waiting */
  /* init the current thread - same "stack group" */
  copy_mem_b(current_thread(),pcd->calling_thread,
             sizeof(clisp_thread_t)); /* ?!! */
  xmutex_unlock(&pcd->mutex); /* allow the main thread to timeout */
  /* unwind if the thread gets cancelled */
  pthread_cleanup_push(&unwind_upto,(void*)&STACK[0]);
  funcall(STACK_0,argcount); /* run body-function */
  pthread_cleanup_pop(0);
  xmutex_lock(&pcd->mutex);
  /* copy the return values &c */
  copy_mem_b(pcd->calling_thread,current_thread(),
             sizeof(clisp_thread_t)); /* ?!! */
  pcd->done = true;
  xmutex_unlock(&pcd->mutex);
  xcondition_broadcast(&pcd->cond);
  return NULL;
}

LISPFUNN(call_with_timeout,3)
{ /* (CALL-WITH-TIMEOUT timeout timeout-function body-function)
 the reason we go with C instead of Lisp is that we save on creating a
 separate STACK for the body thread (i.e., the waiting thread and the
 body thread run in the same "stack group").
 the return values come either from body-function or from timeout-function */
  var struct timeval tv;
  var struct timeval *tvp = sec_usec(STACK_2,unbound,&tv);
  if (tvp) {
    var xthread_t xth;
    var struct call_data_t cd;
    cd.caller = &STACK_0; /* body-function */
    cd.argcount = 0; cd.done = false; cd.calling_thread = current_thread();
    xcondition_init(&cd.cond); xmutex_init(&cd.mutex);
    xmutex_lock(&cd.mutex);
    xthread_create(&xth,&exec_call,(void*)&cd);
    var struct timeval now;
    var struct timespec timeout;
    var int retval=0;
    gettimeofday(&now,NULL);
    timeout.tv_sec = now.tv_sec + tv.tv_sec;
    timeout.tv_nsec = 1000*(now.tv_usec + tv.tv_usec);
    while (!cd.done && retval != ETIMEDOUT)
      retval = xcondition_timedwait(&cd.cond,&cd.mutex,&timeout);
    if (retval == ETIMEDOUT) {
      xthread_cancel(xth);
      funcall(STACK_1,0); /* run timeout-function */
    }
    xcondition_destroy(&cd.cond);
    xmutex_destroy(&cd.mutex);
  } else
    funcall(STACK_1,0);
  skipSTACK(3);
}

LISPFUN(thread_wait,seclass_default,3,0,rest,nokey,0,NIL)
{ /* (THREAD-WAIT whostate timeout predicate &rest arguments)
   predicate may be a LOCK structure in which case we wait for its release
   timeout maybe NIL in which case we wait forever */
  /* set whostate! */
  NOTREACHED;
}

LISPFUNN(thread_yield,0)
{ /* (THREAD-YIELD) */
  NOTREACHED;
}

LISPFUNN(thread_kill,1)
{ /* (THREAD-KILL thread) */
  NOTREACHED;
}

LISPFUN(thread_interrupt,seclass_default,2,0,rest,nokey,0,NIL)
{ /* (THREAD-INTERRUPT thread function &rest arguments) */
  NOTREACHED;
}

LISPFUNN(thread_restart,1)
{ /* (THREAD-RESTART thread) */
  NOTREACHED;
}

LISPFUNN(threadp,1)
{ /* (THREADP object) */
  NOTREACHED;
}

LISPFUNN(thread_name,1)
{ /* (THREAD-NAME thread) */
  NOTREACHED;
}

LISPFUNN(thread_active_p,1)
{ /* (THREAD-ACTIVE-P thread) */
  NOTREACHED;
}

LISPFUNN(thread_state,1)
{ /* (THREAD-STATE thread) */
  NOTREACHED;
}

LISPFUNN(thread_whostate,1)
{ /* (THREAD-WHOSTATE thread) */
  NOTREACHED;
}

LISPFUNN(current_thread,0)
{ /* (CURRENT-THREAD) */
  NOTREACHED;
}

LISPFUNN(list_threads,0)
{ /* (LIST-THREADS) */
  NOTREACHED;
}

#endif  /* MULTITHREAD */
