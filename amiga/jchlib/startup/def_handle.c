#include <exec/types.h>
#include <libraries/dos.h>

/* Only define these by default so that your program (and CLISP) can
 * define it itself (not really useful). */

BPTR Input_handle = NULL;
BPTR Output_handle = NULL;
