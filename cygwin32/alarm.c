#include <windows.h>
#include <signal.h>
#include <sys/time.h>

static HANDLE alarm_thread = NULL;
static DWORD alarm_thread_id;
static struct timeval alarm_date;
static unsigned int alarm_interval;

static DWORD WINAPI do_alarm (LPVOID arg);
static DWORD WINAPI do_alarm(arg)
  LPVOID arg;
  { struct timeval now;
   start:
    gettimeofday(&now,NULL);
    if (now.tv_sec < alarm_date.tv_sec
        || now.tv_sec == alarm_date.tv_sec && now.tv_usec < alarm_date.tv_usec)
      { struct timeval diff;
        diff.tv_sec = alarm_date.tv_sec - now.tv_sec;
        if (alarm_date.tv_usec >= now.tv_usec)
          { diff.tv_usec = alarm_date.tv_usec - now.tv_usec; }
        else
          { diff.tv_usec = 1000000 + alarm_date.tv_usec - now.tv_usec; diff.tv_sec -= 1; }
        Sleep((DWORD)(diff.tv_sec * 1000 + (diff.tv_usec + 500) / 1000));
      }
    if (alarm_interval > 0)
      { alarm_date.tv_usec += alarm_interval;
        alarm_date.tv_sec += alarm_date.tv_usec / 1000000;
        alarm_date.tv_usec = alarm_date.tv_usec % 1000000;
        _raise(SIGALRM);
        goto start;
      }
    else
      { _raise(SIGALRM); alarm_thread = NULL; return 0; }
  }

unsigned int alarm (seconds)
  unsigned int seconds;
  { struct timeval now;
    unsigned int remaining;
    if (alarm_thread == NULL && seconds == 0) return 0;
    gettimeofday(&now,NULL);
    if (alarm_thread != NULL)
      { if (now.tv_sec < alarm_date.tv_sec
            || now.tv_sec == alarm_date.tv_sec && now.tv_usec < alarm_date.tv_usec)
          remaining = (alarm_date.tv_sec - now.tv_sec) - (alarm_date.tv_usec < now.tv_usec);
        else
          remaining = 0;
        TerminateThread(alarm_thread,0); alarm_thread = NULL;
      }
    else
      remaining = 0;
    if (seconds > 0)
      { now.tv_sec += seconds;
        alarm_date = now; alarm_interval = 0;
        alarm_thread = CreateThread(NULL,10000,do_alarm,0,0,&alarm_thread_id);
      }
    return remaining;
  }

unsigned int ualarm (value, interval)
  unsigned int value;
  unsigned int interval;
  { struct timeval now;
    unsigned int remaining;
    if (alarm_thread == NULL && value == 0 && interval == 0) return 0;
    gettimeofday(&now,NULL);
    if (alarm_thread != NULL)
      { if (now.tv_sec < alarm_date.tv_sec
            || now.tv_sec == alarm_date.tv_sec && now.tv_usec < alarm_date.tv_usec)
          remaining = (alarm_date.tv_sec - now.tv_sec)*1000000 + alarm_date.tv_usec - now.tv_usec;
        else
          remaining = 0;
        TerminateThread(alarm_thread,0); alarm_thread = NULL;
      }
    else
      remaining = 0;
    if (value > 0 || interval > 0)
      { now.tv_usec += value;
        now.tv_sec += now.tv_usec / 1000000;
        now.tv_usec = now.tv_usec % 1000000;
        alarm_date = now; alarm_interval = interval;
        alarm_thread = CreateThread(NULL,10000,do_alarm,0,0,&alarm_thread_id);
      }
    return remaining;
  }

#ifdef TEST

#include <stdio.h>

static void handler (int sig)
{ printf("Signal %d caught\n",sig); fflush(stdout); }

int main (int argc, char* argv[])
{
  signal(SIGALRM,handler);
  ualarm(5000000,1000000);
  sleep(10);
  ualarm(0,0);
  sleep(5);
  ualarm(0,100000);
  sleep(3);
  printf("Terminating.\n"); fflush(stdout);
  return 0;
}

#endif

