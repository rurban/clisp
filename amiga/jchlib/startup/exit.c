/* GCC Library
 * Jörg Höhle, 15-Jul-94
 */

#include "defs.h"
#include <exec/ports.h>
#include <libraries/dos.h>
#include <proto/exec.h>
#include <proto/dos.h>

volatile void _exit(LONG);

BOOL _Close_Input = FALSE;
BOOL _Close_Output = FALSE;
BPTR _WBOrigDir = NULL;

volatile void exit(int code)
{
  /* we could zero all handles we free, _WBCurrentDir, In/Output_handle ... */
  if (NULL != DOSBase)
    {
      D(ebug("In exit()\n"));
      if (WBenchMsg != NULL)
	{ /* _WBOrigDir may have been NULL, so don't test this */
	  D(ebug("Unlocking WBDir\n"));
	  UnLock(CurrentDir(_WBOrigDir));
	}
      if (_Close_Input && NULL != Input_handle)
	{
	  D(ebug("Closing Input\n"));
	  Close(Input_handle);
	}
      if (_Close_Output && NULL != Output_handle)
	{
	  D(ebug("Closing Output\n"));
	  Close(Output_handle);
	}
      CloseLibrary(DOSBase);
      DOSBase = NULL;
    }
  if (WBenchMsg != NULL) /* WB startup */
    {
      Forbid();
      ReplyMsg((struct Message *)WBenchMsg);
    }
  _exit(code);
}
