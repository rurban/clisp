#ifndef _avcall_arm_c				/*-*- C -*-*/
#define _avcall_arm_c
/**
  Copyright 1993 Bill Triggs, <Bill.Triggs@inrialpes.fr>

  Copyright 1995 Bruno Haible, <haible@clisp.cons.org>

  This is free software distributed under the GNU General Public
  Licence described in the file COPYING. Contact the author if
  you don't have this or can't live with it. There is ABSOLUTELY
  NO WARRANTY, explicit or implied, on this software.
**/
/*----------------------------------------------------------------------
  !!! THIS ROUTINE MUST BE COMPILED gcc -O !!!

  Foreign function interface for an Acorn Risc Maschine with gcc.

  This calls a C function with an argument list built up using macros
  defined in av_call.h.

  ARM Argument Passing Conventions:

  All arguments, except the first 4 words, are passed on the stack with
  word alignment. Doubles take two words. Structure args are passed as
  true structures embedded in the argument stack. To return a structure,
  the called function copies the return value to the address supplied
  in register "%r0".

  Compile this routine with gcc -O (or -O2 or -g -O) to get the right
  register variables, or use the assembler version.
  ----------------------------------------------------------------------*/
#include "avcall.h.in"

#define RETURN(TYPE,VAL)	(*(TYPE*)l->raddr = (TYPE)(VAL))

int
__builtin_avcall(av_alist* l)
{
  register __avword*	sp	__asm__("r13");  /* C names for registers */
/*register __avword	iret	__asm__("r0"); */
  register __avword	iret2	__asm__("r1");
  register float	fret	__asm__("r0");	/* r0 */
  register double	dret	__asm__("r0");	/* r0,r1 */

  __avword space[__AV_ALIST_WORDS];	/* space for callee's stack frame */
  __avword* argframe = sp;		/* stack offset for argument list */
  int arglen = l->aptr - l->args;
  __avword i;

  for (i = 4; i < arglen; i++)		/* push function args onto stack */
    argframe[i-4] = l->args[i];

				/* call function, pass 4 args in registers */
  i = (*l->func)(l->args[0], l->args[1], l->args[2], l->args[3]);

  switch (l->rtype)			/* save return value */
  {
  case __AVvoid:					break;
  case __AVword:	RETURN(__avword,	i);	break;
  case __AVchar:	RETURN(char,		i);	break;
  case __AVschar:	RETURN(signed char,	i);	break;
  case __AVuchar:	RETURN(unsigned char,	i);	break;
  case __AVshort:	RETURN(short,		i);	break;
  case __AVushort:	RETURN(unsigned short,	i);	break;
  case __AVint:		RETURN(int,		i);	break;
  case __AVuint:	RETURN(unsigned int,	i);	break;
  case __AVlong:	RETURN(long,		i);	break;
  case __AVulong:	RETURN(unsigned long,	i);	break;
  case __AVlonglong:
  case __AVulonglong:
    ((__avword*)l->raddr)[0] = i;
    ((__avword*)l->raddr)[1] = iret2;
    break;
  case __AVfloat:	RETURN(float,		fret);	break;
  case __AVdouble:	RETURN(double,		dret);	break;
  case __AVvoidp:	RETURN(void*,		i);	break;
  case __AVstruct:
    /* NB: On arm, all structure sizes are divisible by 4. */
    if (l->flags & __AV_PCC_STRUCT_RETURN)
    { /* pcc struct return convention: need a  *(TYPE*)l->raddr = *(TYPE*)i;  */
      switch (l->rsize)
      {
      case sizeof(char):  RETURN(char,	*(char*)i);	break; /* can't occur */
      case sizeof(short): RETURN(short,	*(short*)i);	break; /* can't occur */
      case sizeof(int):	  RETURN(int,	*(int*)i);	break;
      case sizeof(double):
	((int*)l->raddr)[0] = ((int*)i)[0];
	((int*)l->raddr)[1] = ((int*)i)[1];
	break;
      default:
	{
	  int n = (l->rsize+3)/4;
	  while (--n >= 0)
	    ((__avword*)l->raddr)[n] = ((__avword*)i)[n];
	}
	break;
      }
    }
    else
    { /* normal struct return convention */
      if (l->flags & __AV_REGISTER_STRUCT_RETURN)
	switch (l->rsize)
	{
	case sizeof(char):  RETURN(char,  i);	break; /* can't occur */
	case sizeof(short): RETURN(short, i);	break; /* can't occur */
	case sizeof(int):   RETURN(int,   i);	break;
	case 2*sizeof(__avword):
	  ((__avword*)l->raddr)[0] = i;
	  ((__avword*)l->raddr)[1] = iret2;
	  break;
	default:				break;
	}
    }
    break;
  default:					break;
  }
  return 0;
}

#endif /*_avcall_arm_c */
