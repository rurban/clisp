#ifndef _avcall_rs6000_c				/*-*- C -*-*/
#define _avcall_rs6000_c
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

  Foreign function interface for an IBM RS/6000 with gcc

  This calls a C function with an argument list built up using macros
  defined in av_call.h.

  RS6000 Argument Passing Conventions:

  All arguments, except the first 8 words, are passed on the stack with
  word alignment. Doubles take two words. The first 13 doubles and floats
  are also passed in floating-point-registers.
  To return a structure, the called function copies the value to space
  pointed to by its first argument, and all other arguments are shifted
  down by one.

  Compile this routine with gcc -O (or -O2 -fno-omit-frame-pointer or -g -O)
  to get the right register variables. For other compilers use the
  pre-compiled assembler version.
  ----------------------------------------------------------------------*/
#include "avcall.h.in"

#define RETURN(TYPE,VAL)	(*(TYPE*)l->raddr = (TYPE)(VAL))

register double farg1	__asm__("fr1");
register double farg2	__asm__("fr2");
register double farg3	__asm__("fr3");
register double farg4	__asm__("fr4");
register double farg5	__asm__("fr5");
register double farg6	__asm__("fr6");
register double farg7	__asm__("fr7");
register double farg8	__asm__("fr8");
register double farg9	__asm__("fr9");
register double farg10	__asm__("fr10");
register double farg11	__asm__("fr11");
register double farg12	__asm__("fr12");
register double farg13	__asm__("fr13");

int
__builtin_avcall(av_alist* l)
{
  register __avword*	sp	__asm__("r1");  /* C names for registers */
/*register __avword	iret	__asm__("r3"); */
  register __avword	iret2	__asm__("r4");
  register float	fret	__asm__("fr1");
  register double	dret	__asm__("fr1");

  __avword space[__AV_ALIST_WORDS];	/* space for callee's stack frame */
  __avword* argframe = sp + 14;		/* stack offset for argument list */
  int arglen = l->aptr - l->args;
  __avword i;

  for (i = 8; i < arglen; i++)		/* push function args onto stack */
    argframe[i-8] = l->args[i];

			/* pass first 13 floating-point args in registers */
  switch (l->faptr - l->fargs)
    { default:
      case 13: farg13 = l->fargs[12];
      case 12: farg12 = l->fargs[11];
      case 11: farg11 = l->fargs[10];
      case 10: farg10 = l->fargs[9];
      case  9: farg9 = l->fargs[8];
      case  8: farg8 = l->fargs[7];
      case  7: farg7 = l->fargs[6];
      case  6: farg6 = l->fargs[5];
      case  5: farg5 = l->fargs[4];
      case  4: farg4 = l->fargs[3];
      case  3: farg3 = l->fargs[2];
      case  2: farg2 = l->fargs[1];
      case  1: farg1 = l->fargs[0];
      case  0: ;
    }
				/* call function, pass 8 args in registers */
  i = (*l->func)(l->args[0], l->args[1], l->args[2], l->args[3],
		 l->args[4], l->args[5], l->args[6], l->args[7]);

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
      if (l->flags & __AV_REGISTER_STRUCT_RETURN)
	switch (l->rsize)
	{
	case sizeof(char):  RETURN(char,  i);	break;
	case sizeof(short): RETURN(short, i);	break;
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

#endif /*_avcall_rs6000_c */
