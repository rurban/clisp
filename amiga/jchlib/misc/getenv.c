/* Tiny GCC Library
 * Jörg Höhle, 12-Jun-96
 */

#include <exec/types.h>
#include <dos/dosextens.h>
#include <dos/var.h>

#include <proto/dos.h>

#define GETENV_MAX_LENGTH 256	/* won't handle values longer than this */

static char getenv_static_buf[GETENV_MAX_LENGTH];

char *getenv(const char *name)
{
  LONG len;
  extern struct DosLibrary * const DOSBase;

#ifdef SUPPORT_1_3
  if (DOSBase->dl_lib.lib_Version < 36) return NULL; /* TODO add it for 1.3 */
#endif
  len =  GetVar(name, getenv_static_buf, GETENV_MAX_LENGTH, LV_VAR);
  return (len >= 0 && len == IoErr() /* not truncated */)
    ? getenv_static_buf : NULL;
}
