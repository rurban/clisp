#ifndef _avcall_m68k_amiga_c				/*-*- C -*-*/
#define _avcall_m68k_amiga_c
/**
  Copyright 1993 Bill Triggs, <Bill.Triggs@inrialpes.fr>

  Copyright 1995 Bruno Haible, <haible@clisp.cons.org>

  Copyright 1997 Jörg Höhle, <Joerg.Hoehle@gmd.de>

  This is free software distributed under the GNU General Public
  Licence described in the file COPYING. Contact the author if
  you don't have this or can't live with it. There is ABSOLUTELY
  NO WARRANTY, explicit or implied, on this software.
**/
/*----------------------------------------------------------------------
  !!! THIS ROUTINE MUST BE COMPILED gcc -O !!!

  Foreign function interface for a m68k Amiga with gcc.

  This calls a C function with an argument list built up using macros
  defined in av_call.h.

  M68k Argument Passing Conventions:

  All arguments are passed on the stack with word alignment. Doubles take
  two words. Structure args are passed as true structures embedded in the
  argument stack. To return a structure, the called function copies the
  return value to the address supplied in register "a1". Gcc without
  -fpcc-struct-return returns <= 4 byte structures as integers.

  Some specific arguments may be passed in registers.

  Compile this routine with gcc -O (or -O2 or -g -O) to get the right
  register variables, or use the assembler version.
  ----------------------------------------------------------------------*/
#include "avcall.h.in"

#define RETURN(TYPE,VAL)	(*(TYPE*)l->raddr = (TYPE)(VAL))

int
__builtin_avcall(av_alist* l)
{
  register __avword*	sp	__asm__("sp");  /* C names for registers */
  register __avword*	sret	__asm__("a1");	/* structure return pointer */
  register __avword	iret	__asm__("d0");
  register __avword	iret2	__asm__("d1");
  register float	fret	__asm__("d0");	/* d0 */
  register double	dret	__asm__("d0");	/* d0,d1 */

  #define __NUM_SAVED_REGS	(6+7)	/* save d2-d7/a0-a6 */
  #define __SAVED_REGS		0x7ffc	/* moveml mask for d2-d7/a0-a6 */
  /* space for callee's stack frame and temp space for saving registers */
  sp -= __AV_ALIST_WORDS + __NUM_SAVED_REGS;
  #define regspace (sp+__AV_ALIST_WORDS) /* temp space for saving registers */

  {
    __avword* argframe = sp;		/* stack offset for argument list */
    int arglen = l->aptr - l->args;
    int i;

    for (i = 0; i < arglen; i++)	/* push function args onto stack */
      argframe[i] = l->args[i];
  }

  if (l->rtype == __AVstruct)		/* put struct return address into a1 */
    l->regargs[8+1] = (__avword)(l->raddr);

  /* Save most registers by hand. There is no way to persuade gcc that
   * they are clobbered, no matter how large we make the clobber list of
   * of the asm statements.
   */
  __asm__("moveml %1,%0" /* %1 == #0x7ffc == d2-d7/a0-a6 */
          : "=m" (regspace[0]) : "n" (__SAVED_REGS));

  __asm__("movel %0,sp@-" : : "g" (&&return_here)); /* prepare function call */
  __asm__("movel %0,sp@-" : : "g" (l->func));

					/* put some arguments into registers */
  __asm__("moveml %0,#x7fff" /* 0x7fff == d0-d7/a0-a6 */
          :
          : "m" (l->regargs[0])
          /* no need for a clobber list since we save the registers ourselves */
         );

  __asm__("rts" : "=d" (iret) :  : "d1");	/* call function */
  return_here:					/* function returns here */

  __asm__("moveml %0,%1" /* %1 == #0x7ffc == d2-d7/a0-a6 */
          : : "m" (regspace[0]), "n" (__SAVED_REGS) );	/* restore registers */

  sp += __AV_ALIST_WORDS + __NUM_SAVED_REGS;	/* restore stack pointer */

  switch (l->rtype)			/* save return value */
  {
  case __AVvoid:					break;
  case __AVword:	RETURN(__avword,	iret);	break;
  case __AVchar:	RETURN(char,		iret);	break;
  case __AVschar:	RETURN(signed char,	iret);	break;
  case __AVuchar:	RETURN(unsigned char,	iret);	break;
  case __AVshort:	RETURN(short,		iret);	break;
  case __AVushort:	RETURN(unsigned short,	iret);	break;
  case __AVint:		RETURN(int,		iret);	break;
  case __AVuint:	RETURN(unsigned int,	iret);	break;
  case __AVlong:	RETURN(long,		iret);	break;
  case __AVulong:	RETURN(unsigned long,	iret);	break;
  case __AVlonglong:
  case __AVulonglong:
    ((__avword*)l->raddr)[0] = iret;
    ((__avword*)l->raddr)[1] = iret2;
    break;
  case __AVfloat:
    if (l->flags & __AV_SUNCC_FLOAT_RETURN)
      RETURN(float, (float)dret);
    else
      RETURN(float, fret);
    break;
  case __AVdouble:	RETURN(double,		dret);	break;
  case __AVvoidp:	RETURN(void*,		iret);	break;
  case __AVstruct:
    /* NB: On m68k, all structure sizes are divisible by 2. */
    if (l->flags & __AV_REGISTER_STRUCT_RETURN)
      switch (l->rsize)
      {
	case sizeof(char):  RETURN(char,  iret); goto done; /* can't occur */
	case sizeof(short): RETURN(short, iret); goto done;
	case sizeof(int):   RETURN(int,   iret); goto done;
	case 2*sizeof(__avword):
	  ((__avword*)l->raddr)[0] = iret;
	  ((__avword*)l->raddr)[1] = iret2;
	  goto done;
	default:				break;
      }
    if (l->flags & __AV_PCC_STRUCT_RETURN)
    { /* pcc struct return convention: need a  *(TYPE*)l->raddr = *(TYPE*)i;  */
      switch (l->rsize)
      {
      case sizeof(char):  RETURN(char,	*(char*)iret);	break; /* can't occur */
      case sizeof(short): RETURN(short,	*(short*)iret);	break;
      case sizeof(int):	  RETURN(int,	*(int*)iret);	break;
      case sizeof(double):
	((int*)l->raddr)[0] = ((int*)iret)[0];
	((int*)l->raddr)[1] = ((int*)iret)[1];
	break;
      default:
	{
	  int n = (l->rsize + sizeof(__avword)-1)/sizeof(__avword);
	  while (--n >= 0)
	    ((__avword*)l->raddr)[n] = ((__avword*)iret)[n];
	}
	break;
      }
    }
    else
    { /* normal struct return convention */ }
    break;
  default:						break;
  }
 done:
  return 0;
}

#endif /*_avcall_m68k_amiga_c */
