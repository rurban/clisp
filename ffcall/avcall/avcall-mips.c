#ifndef _avcall_mips_c				/*-*- C -*-*/
#define _avcall_mips_c
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

  Foreign function interface for an SGI MIPS with gcc/sgi-cc.

  This calls a C function with an argument list built up using macros
  defined in av_call.h.

  SGI MIPS Argument Passing Conventions

  - The entire argument list forms a structure with all the appropriate
    holes & alignments, and space for this is allocated in the stack frame.
  - Shorter integers are promoted to word length (sizeof(int)=sizeof(long)=4).
  - Doubles are 2 words aligned on even boundaries.
  - The first 4 words of the structure are passed in registers $4...$7, stack
    space for these is always allocated. Remaining words are passed on the
    stack.
  - If the first two args are floats or doubles, they are also passed in $f12
    and $f14. But varargs functions will expect them in the integer registers
    and we can't tell whether the function is varargs so we pass them both ways.
  - Structure arguments are copies embedded in the arglist structure.
  - Structure returns are pointers to caller-allocated space passed in as the
    first argument of the list. The function also returns the pointer.
  - Integer/pointer returns are in $2, float/double returns in $f0.
  - Under IRIX 5, the called function expects to see its own address in $25.

  This file needs to be compiled with gcc for the asm extensions, but the
  assembly version of it and the header file seem to work with SGI cc.
  ----------------------------------------------------------------------*/
#include "avcall.h.in"

#define RETURN(TYPE,VAL)	(*(TYPE*)l->raddr = (TYPE)(VAL))
#define OFFSETOF(struct,member) ((int)&(((struct*)0)->member))

typedef __avword (*func_pointer)();
register func_pointer	t9	__asm__("$25");

int
__builtin_avcall(av_alist* l)
{
  register __avword*	sp	__asm__("$sp");  /* C names for registers */
  register __avword	iret2	__asm__("$3");
  register float	fret	__asm__("$f0");
  register double	dret	__asm__("$f0");
  __avword *space = __builtin_alloca(__AV_ALIST_WORDS * sizeof(__avword));	/* big space for child's stack frame */
  __avword *argframe = (__avword*)sp;	/* stack offset for argument list is 0 */
  int arglen = l->aptr - l->args;
  __avword i;

  if (l->flags & __AV_FLOAT_1)		/* push leading float args */
  {
    __asm__("l.d $f12,%1(%0)" : : "p" (l), "i" OFFSETOF(av_alist,floatarg[0]));
    if (l->flags & __AV_FLOAT_2)
      __asm__("l.d $f14,%1(%0)" : : "p" (l), "i" OFFSETOF(av_alist,floatarg[1]));
  }

  for (i = 4; i < arglen; i++)		/* push excess function args */
    argframe[i] = l->args[i];

  i = (*(t9 = l->func))(l->args[0], l->args[1],  /* call function with 1st 4 args */
			l->args[2], l->args[3]);

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
    if (l->flags & __AV_PCC_STRUCT_RETURN)
    { /* pcc struct return convention: need a  *(TYPE*)l->raddr = *(TYPE*)i;  */
      switch (l->rsize)
      {
      case sizeof(char):  RETURN(char,	*(char*)i);	break;
      case sizeof(short): RETURN(short,	*(short*)i);	break;
      case sizeof(int):	  RETURN(int,	*(int*)i);	break;
      case sizeof(double):
	((int*)l->raddr)[0] = ((int*)i)[0];
	((int*)l->raddr)[1] = ((int*)i)[1];
	break;
      default:
	{
	  int n = (l->rsize + sizeof(__avword)-1)/sizeof(__avword);
	  while (--n >= 0)
	    ((__avword*)l->raddr)[n] = ((__avword*)i)[n];
	}
	break;
      }
    }
    else
    { /* normal struct return convention */
      if (l->flags & __AV_SMALL_STRUCT_RETURN)
	switch (l->rsize)
	{
	case sizeof(char):  RETURN(char,  i);	break;
	case sizeof(short): RETURN(short, i);	break;
	case sizeof(int):   RETURN(int,   i);	break;
	default:				break;
	}
    }
    break;
  default:					break;
  }
  return 0;
}

#endif /*_avcall_mips_c */
