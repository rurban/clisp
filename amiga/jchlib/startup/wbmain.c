/* Tiny GCC Library
 * Jörg Höhle, 9-Jul-96
 */

#include "defs.h"
#include <exec/libraries.h>
#include <workbench/startup.h>
#include <workbench/icon.h>
#include <workbench/workbench.h>

struct Library * IconBase = NULL;

#include <proto/exec.h>
#include <proto/dos.h>
#define BASE_EXT_DECL	/* no const declaration with old GCC function inlines */
#include <proto/icon.h>

/* FALSE for standard code (def_window.c), TRUE for CLISP (through clisp.c) */
extern BOOL  _CLISP_glue;

/* These are pulled from the library unless you define them (see clisp.c) */
#ifdef SUPPORT_1_3
extern UBYTE _WDefName13[];
#endif
extern UBYTE _WDefName[];

void wbmain(struct WBStartup* wbmsg)
{
  UBYTE *ioname = NULL;  /* IO stream name that we are going to open */
  int argc = 0;
  char **argv;
  short int numargs = wbmsg->sm_NumArgs;
  #define MAXIONAME 100 /* including '\0' byte */
  UBYTE ionamebuf[MAXIONAME];
  struct argline {
    char *line;       /* tokenized content */
    int nt;           /* number of tokens in ARGS ToolType line */
  } argline[numargs];
  D(ebug("In wbmain(), numargs=%ld\n",numargs));
  IconBase = OpenLibrary(ICONNAME,0L);
  if (IconBase != NULL)
    { register short int i;
      struct WBArg *wbarg;
      for (i = 0, wbarg = wbmsg->sm_ArgList;
           i < numargs;
           i++, wbarg++)
        { struct DiskObject* dobj;
          argline[i].line = NULL;
	  D(ebug("Got an object "));
          if (wbarg->wa_Name != NULL)
            {
              dobj = GetDiskObject(wbarg->wa_Name);
	      D(ebug(wbarg->wa_Name));
            }
          else { dobj = NULL; D(ebug("without name!")); }
          if (dobj != NULL)
            { char* arg = FindToolType(dobj->do_ToolTypes,"WINDOW");
              /* ex: WINDOW=CON:0/0/640/200/CLOS-Listener/CLOSE oder WINDOW=NIL: */
              if (arg != NULL)
                { register int len = strlen(arg);
                  if (len < MAXIONAME)
                    { /* we overwrite, thus using the last possible name */
		      D(ebug("Found WINDOW tooltype\n"));
                      ioname = &ionamebuf[0];
                      nzbcopy(arg,ionamebuf,len+1);
                    }
                }
              arg = FindToolType(dobj->do_ToolTypes,"ARGS");
              /* ex: ARGS=-Mcompiled.mem */
              if (arg != NULL)
                { register int len = strlen(arg);
		  D(ebug("Found ARGS tooltype: %s\n",arg));
                  argline[i].line = alloca(len+1);
                  nzbcopy(arg,argline[i].line,len+1);
                  argc += argline[i].nt = _tokenize(argline[i].line,len);
                }
              FreeDiskObject(dobj);
            }
          if (i > 0 && argline[i].line == NULL)
            { /* if there's no ARGS ToolType don't resign, add the filename */
	      if (_CLISP_glue) argc++; /* add -i filename */
	      argc++;
            }
        }
      CloseLibrary(IconBase);
      IconBase = NULL;
    }
  D(ebug("Argc is now %ld\n",argc));
  D(ebug("Going to alloca(argv)\n"));
  argv = alloca((argc+2) * sizeof(char *));  /* room for argv[0] and last NULL */
  argv[0] = (char *)(wbmsg->sm_ArgList->wa_Name);  /* command name first */
  /* (we fail to consider the case where sm_ArgList is NULL) */
  { register short int i;
    struct WBArg *wbarg;
    register char **av = &argv[1];
    for(i = 0, wbarg = wbmsg->sm_ArgList;
        i < numargs;
        i++, wbarg++)
      {
        if (argline[i].line != NULL)
          { register int ac = argline[i].nt;
	    D(ebug("ARGS argline, ac is %ld\n",ac));
            _dumpargs(argline[i].line,av,ac);
            for ( ; ac > 0; ac--, av++)
              { /* replace the token '*' with the file name */
                if ((*av)[0] == '*' && (*av)[1] == '\0')
                  { *av = wbarg->wa_Name; }
              }
          }
        else if (i > 0) /* not for the command name */
          { /* no ARGS option specified, use "-i <name>" (see above) */
	    if (_CLISP_glue) *(av++) = "-i";
            *(av++) = wbarg->wa_Name;
	    D(ebug("Empty argline\n"));
          }
      }
    *av = NULL;
    /* av - &argv[1] should be argc now */
    D(ebug("+ av %ld\n",av - &argv[1]));
  }
  D(ebug(argv[0]));
  argc++;       /* count command name argv[0] */

  if (ioname == NULL)
#ifdef SUPPORT_1_3
    { ioname = _OS_Version >= 37 ? _WDefName : _WDefName13; }
#else
    { ioname = _WDefName; }
#endif

  Input_handle = Output_handle = Open(ioname,MODE_OLDFILE);
  if (NULL != Input_handle)
    {
      _Close_Output = TRUE;     /* opened once, so close once */
      D(ebug("Calling main()\n"));
      main(argc,argv);
    }
}
