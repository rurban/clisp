#include <exec/types.h>
#include <exec/execbase.h>
#include <dos/dosextens.h>

#define __CONSTLIBBASEDECL__ const /* for GCC inlines */
extern struct ExecBase * const SysBase;
#include <inline/exec.h>
#include <inline/dos.h>

/* From Ralph Babel, The Amiga GURU book, p. 278 */
/* SetMode() for pre-2.0 systems		 */

typedef struct StandardPacket StdPkt;

LONG setmode(BPTR fh, LONG mode)
{extern struct DosLibrary * const DOSBase;
 if (DOSBase->dl_lib.lib_Version > 35)
   return SetMode(fh,mode);
 {register struct MsgPort *fh_type = ((struct FileHandle *)BADDR(fh))->fh_Type;
  if (NULL == fh_type) return DOSFALSE; /* NIL: has no message port */
		       /* should also set Result2 */
  else
    { struct MsgPort *mp;
      char SP[sizeof(StdPkt) + 2]; /* LONG-align, 2 is enough since stack is even */
      register StdPkt *sp = (StdPkt *)((ULONG)(SP + 2) & ~3);

      mp = &((struct Process *)FindTask(NULL))->pr_MsgPort;

      sp->sp_Msg.mn_Node.ln_Name = (char *)&sp->sp_Pkt;
      sp->sp_Pkt.dp_Link	 = &sp->sp_Msg;
      sp->sp_Pkt.dp_Port	 = mp;
      sp->sp_Pkt.dp_Type	 = ACTION_SCREEN_MODE;
      sp->sp_Pkt.dp_Arg1	 = mode; /* DOSFALSE (0) for CON */

      PutMsg(fh_type, &sp->sp_Msg);
      (void)WaitPort(mp);
      (void)GetMsg(mp);		/* assumes that no other packets are pending */
      return sp->sp_Pkt.dp_Res1;
    }
}}


#if defined(MAIN) && defined(JCHLIB) /* make a very small executable */

/*struct ExecBase * SysBase;*/
struct DosLibrary * DOSBase = NULL;
/* UWORD _OS_Version; */

/* With no arg, set CON: mode, else RAW: on the Input() filehandle */

LONG _main(LONG arglen, UBYTE* arg)
{
  LONG res;
  if (NULL == (DOSBase = (struct DosLibrary *)OpenLibrary(DOSNAME,0L)))
    { return 20; }	/* don't use exit()! */
  res = setmode(Input(), arglen > 1 ? DOSTRUE : DOSFALSE);
  CloseLibrary((struct Library *)DOSBase);
  return res /* DOSTRUE */ ? 0 : 5;
}
#endif
