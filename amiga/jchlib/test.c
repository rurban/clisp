/* test file for the Tiny GCC Library */
#include <exec/types.h>
#include <libraries/dos.h>
#include <stdlib.h>
#include <string.h>
#include <libgoodies.h>	/* for BPTRfprintf() */

#define __CONSTLIBBASEDECL__ const /* better code with GCC inlines */
extern struct DosLibrary * const DOSBase;
#include <proto/dos.h>

/* This comes from the startup code */
extern BPTR Output_handle;

const char *usage = "Usage:
Please try this from WorkBench also, using the ARGS tooltype."
#if 0
"When called with four arguments, does setjmp() test."
#endif
;

int main(int argc, char *argv[])
{
  /* the startup code should have initialised this */
  if (Output_handle == NULL) exit(RETURN_FAIL);

  Write(Output_handle,"Hello World!\n",13);
  switch (argc)
    {
    case 0:
      Write(Output_handle,"argc is 0!",10);
      break;
    default:
      BPTRfprintf(Output_handle,"Argc is %ld:\n",argc);
    }
  if (argc > 0)
    { int i;
      for (i=0; i<argc; i++)
        { BPTRfprintf(Output_handle,"%ld: %s\n",i,argv[i]); }
    }
  if (argc == 1)
    { Write(Output_handle,usage,strlen(usage)); }
  /* TODO argc=5 and setjmp() test */
  exit(RETURN_WARN-1); /* test return code */
}
