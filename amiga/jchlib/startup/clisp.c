#include <exec/types.h>
#include <libraries/dos.h>

/* for inclusion with CLISP: wbmain() does specials */
BOOL _CLISP_glue = TRUE;

/* CLISP uses these handles that are defined by the startup code */
/* BPTR Input_handle = NULL;  */
/* BPTR Output_handle = NULL; */

#ifdef SUPPORT_1_3
UBYTE _WDefName13[] = "CON:0/11/640/186/CLISP-Listener";
#endif
UBYTE _WDefName[] =  "CON:0/11//186/CLISP-Listener/CLOSE/AUTO/WAIT";
