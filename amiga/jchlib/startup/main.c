/* Tiny GCC Library
 * Jörg Höhle, 15-Jul-94
 */

#include "defs.h"
#include <libraries/dos.h>
#include <libraries/dosextens.h>
#include <exec/ports.h>
#include <workbench/startup.h>
#include <proto/exec.h>
#include <proto/dos.h>

struct ExecBase * const SysBase;
struct DosLibrary * DOSBase = NULL;
struct WBStartup * WBenchMsg = NULL;
#ifdef SUPPORT_1_3
UWORD _OS_Version;
#define MINIMAL_VERSION 0L
#else
#define MINIMAL_VERSION 37L
#endif

long _main(LONG arglen, UBYTE* arg)
{
  register struct Process *proc = (struct Process *)FindTask(NULL);
#ifdef SUPPORT_1_3
  _OS_Version = SysBase->LibNode.lib_Version;
#endif
  if (NULL == proc->pr_CLI) /* WB startup */
    {
      WaitPort(&proc->pr_MsgPort);
      WBenchMsg = (struct WBStartup *)GetMsg(&proc->pr_MsgPort);
    }

  if (NULL == (DOSBase = (struct DosLibrary *)OpenLibrary(DOSNAME,MINIMAL_VERSION)))
    { exit(-1L); } /* DOSBase will not necessarily be set in exit()! */

  if (NULL != proc->pr_CLI) /* CLI startup */
    { register int argc;
      char argcopy[arglen+1];   /* gcc allows dynamic allocation */
      bcopy(arg,argcopy,arglen);
      argcopy[arglen] = '\0';
      argc = _tokenize(argcopy,arglen);
      {
        char *argv[argc+2];     /* room for argv[0] and last NULL */
        _dumpargs(argcopy,&argv[1],argc);
        ++argc;                 /* count commandname argv[0] */
        { char *cname;
          struct CommandLineInterface *cli
            = (struct CommandLineInterface *)BADDR(proc->pr_CLI);
          if (NULL != (cname = (char *)(BADDR(cli->cli_CommandName))))
            {
              arglen = *(cname++);      /* BCPL string contains length at [0] */
              argv[0] = alloca(arglen+1);
              bcopy(cname,argv[0],arglen);
              argv[0][arglen] = '\0';
            }
          else { argv[0] = ""; }
        }
        argv[argc] = NULL;      /* put NULL at end of argv array */
        Input_handle = Input();
        Output_handle = Output();
        exit(main(argc,argv));
      }
    }
  else /* WB startup */
    {
      _WBOrigDir = CurrentDir(DupLock(WBenchMsg->sm_ArgList->wa_Lock));
      if (_WBOrigDir == NULL) { D(ebug("WBDir is NULL")); }
      D(ebug("Calling wbmain()\n"));
      wbmain(WBenchMsg);
      exit(0);
    }
}
