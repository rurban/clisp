/* GCC Library
 * Jörg Höhle, 12-Jun-96
 */

#include <exec/types.h>
#include <exec/ports.h>
#include <exec/memory.h>

#define __CONSTLIBBASEDECL__ const /* for GCC inlines */
extern struct ExecBase * const SysBase;
#include <proto/exec.h>
#include <proto/alib.h>	/* for NewList() */

void DeletePort(struct MsgPort * port);
void DeletePort(port)
struct MsgPort* port;
{
  if (port != NULL)
    {
      if (port->mp_Node.ln_Name)
	{ RemPort(port); }
      if (port->mp_Flags == PA_SIGNAL)
        { FreeSignal(port->mp_SigBit); }
      FreeMem(port,sizeof(struct MsgPort));
    }
}

struct MsgPort * CreatePort(UBYTE * name, long priority);
struct MsgPort * CreatePort(name, pri)
UBYTE* name;
long pri;
{
  register struct MsgPort* port;
  if ((port = (struct MsgPort *)AllocMem(sizeof(struct MsgPort),MEMF_PUBLIC | MEMF_CLEAR)) != NULL)
    {
      if ((BYTE)(port->mp_SigBit = AllocSignal(-1L)) >= 0)
        {
          port->mp_Node.ln_Name = name;
          port->mp_Node.ln_Type = NT_MSGPORT;
          port->mp_Node.ln_Pri = pri;
          port->mp_SigTask = FindTask(NULL);
          port->mp_Flags = PA_SIGNAL;
          NewList(&port->mp_MsgList);
          if (name != NULL)
	    { AddPort(port); }
        }
      else
        {
          FreeMem(port,sizeof(struct MsgPort));
          port = NULL;
        }
    }
  return port;
}
