#include <windows.h>
#include <signal.h>
#include <sys/time.h>

static BOOL DoInterruptible (LPTHREAD_START_ROUTINE fn, LPVOID arg);
static HANDLE interruptible_thread;
static BOOL interrupt_handler (DWORD CtrlType);
static BOOL interrupt_handler(CtrlType)
  DWORD CtrlType;
  { if (CtrlType == CTRL_C_EVENT || CtrlType == CTRL_BREAK_EVENT)
      { // Terminate the interruptible operation, set the exitcode to 1.
        TerminateThread(interruptible_thread,1);
        // Invoke signal handler.
        _raise(SIGINT);
        // Don't invoke the other handlers (in particular, the default handler)
        return TRUE;
      }
    else
      // Do invoke the other handlers.
      return FALSE;
  }
static BOOL DoInterruptible(fn,arg)
  LPTHREAD_START_ROUTINE fn;
  LPVOID arg;
  { HANDLE thread;
    DWORD thread_id;
    DWORD thread_exitcode;
    thread = CreateThread(NULL,10000,fn,arg,0,&thread_id);
    if (thread == NULL) return FALSE;
    interruptible_thread = thread;
    SetConsoleCtrlHandler((PHANDLER_ROUTINE)interrupt_handler,TRUE);
    WaitForSingleObject(interruptible_thread,INFINITE);
    SetConsoleCtrlHandler((PHANDLER_ROUTINE)interrupt_handler,FALSE);
    GetExitCodeThread(interruptible_thread,&thread_exitcode);
    if (thread_exitcode==0)
      return TRUE; // successful termination
    else
      return FALSE;
  }
static DWORD WINAPI do_sleep(arg)
  LPVOID arg;
  { Sleep((DWORD)arg); return 0; }

unsigned int sleep (unsigned int seconds)
  { struct timeval target_time;
    struct timeval end_time;
    gettimeofday(&target_time,NULL); target_time.tv_sec += seconds;
    DoInterruptible(&do_sleep,(void*)(seconds * 1000));
    gettimeofday(&end_time,NULL);
    if (end_time.tv_sec < target_time.tv_sec
        || end_time.tv_sec == target_time.tv_sec && end_time.tv_usec < target_time.tv_usec)
      return (target_time.tv_sec - end_time.tv_sec) - (target_time.tv_usec < end_time.tv_usec);
    else
      return 0;
  }

unsigned int usleep (unsigned int useconds)
  { struct timeval target_time;
    struct timeval end_time;
    gettimeofday(&target_time,NULL);
    target_time.tv_usec += useconds;
    target_time.tv_sec += target_time.tv_usec / 1000000;
    target_time.tv_usec = target_time.tv_usec % 1000000;
    DoInterruptible(&do_sleep,(void*)((useconds + 500) / 1000));
    gettimeofday(&end_time,NULL);
    if (end_time.tv_sec < target_time.tv_sec
        || end_time.tv_sec == target_time.tv_sec && end_time.tv_usec < target_time.tv_usec)
      return (target_time.tv_sec - end_time.tv_sec)*1000000 + target_time.tv_usec - end_time.tv_usec;
    else
      return 0;
  }

#ifdef TEST

#include <stdio.h>

static void handler (int sig)
{ printf("Signal %d caught\n",sig); fflush(stdout); }

int main (int argc, char* argv[])
{
  signal(SIGINT,handler);
  printf("%d\n",sleep(10));
  return 0;
}

#endif

