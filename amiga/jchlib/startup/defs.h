/* Tiny GCC Library
 * Jörg Höhle, 21-Jul-94
 */

#include <exec/types.h>
#include <exec/execbase.h>
#include <libraries/dosextens.h>
#include <workbench/startup.h>

/* dbra-optimized version that works with with int vars for gcc1.40,2.33,2.5x,2.6 */
#define dotimes(dotimesvar,num,statement)  \
{ dotimesvar = (num);                 \
  if (!(dotimesvar==0))                \
    { dotimesvar--;                     \
      do {statement}                     \
         while ((int)(--dotimesvar)!=-1); \
}   }

#define bcopy(from,to,num)  \
{ register char* top = (to);          \
  register const char* fromp = (from); \
  register int bcopycount;              \
  dotimes(bcopycount,(num),              \
	  { *top++ = *fromp++; } );       \
}

/* integrated version, but num mustn't be 0 */
#define nzbcopy(from,to,num)  \
{ register char* top = (to);          \
  register const char* fromp = (from); \
  register int bcopycount = (num)-1;    \
  do { *top++ = *fromp++; }              \
     while (--bcopycount!=-1);            \
}

#define alloca __builtin_alloca
#if __GNUC__ >= 2
#define strlen __builtin_strlen
#endif

volatile void exit(int);
int main(int argc, char ** argv);
void wbmain(struct WBStartup * wbmsg);
int _tokenize(volatile char * copy, int len);
void _dumpargs(char * copy, char ** argv, int argc);
int strlen (const char *);

/* debugging only */
#ifdef DEBUG
/* Use as D(ebug("%ld\n",number)); */
#define ebug kprintf
#define D(thing) thing
#else
#define D(thing)
#endif

/* Variables defined by startup */
extern struct ExecBase * SysBase;	/* main.c */
extern struct DosLibrary * DOSBase;	/* main.c */
extern struct WBStartup * WBenchMsg;	/* main.c */
extern BPTR _WBOrigDir;			/* exit.c */
extern BPTR Input_handle;		/* clisp.c */
extern BPTR Output_handle;		/* clisp.c */
extern BOOL _Close_Input;		/* exit.c */
extern BOOL _Close_Output;		/* exit.c */
extern UWORD _OS_Version;		/* main.c */
/*extern struct Library * IconBase	 * wbmain.c */
