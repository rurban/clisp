#ifndef _avcall_m68k_c				/*-*- C -*-*/
#define _avcall_m68k_c
/**
  Copyright 1993 Bill Triggs, <Bill.Triggs@inrialpes.fr>

  Copyright 1995-1999 Bruno Haible, <haible@clisp.cons.org>

  This is free software distributed under the GNU General Public
  Licence described in the file COPYING. Contact the author if
  you don't have this or can't live with it. There is ABSOLUTELY
  NO WARRANTY, explicit or implied, on this software.
**/
/*----------------------------------------------------------------------
  !!! THIS ROUTINE MUST BE COMPILED gcc -O !!!

  Foreign function interface for a m68k Sun3 with gcc/sun-cc.

  This calls a C function with an argument list built up using macros
  defined in av_call.h.

  M68k Argument Passing Conventions:

  All arguments are passed on the stack with word alignment. Doubles take
  two words. Structure args are passed as true structures embedded in the
  argument stack. To return a structure, the called function copies the
  return value to the address supplied in register "a1". Gcc without
  -fpcc-struct-return returns <= 4 byte structures as integers.

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
/*register __avword	iret	__asm__("d0"); */
  register __avword	iret2	__asm__("d1");
  register float	fret	__asm__("d0");	/* d0 */
  register double	dret	__asm__("d0");	/* d0,d1 */
  register float	fp_fret	__asm__("fp0");
  register double	fp_dret	__asm__("fp0");

  __avword* argframe = (sp -= __AV_ALIST_WORDS); /* make room for argument list */
  int arglen = l->aptr - l->args;
  __avword i;

  for (i = 0; i < arglen; i++)		/* push function args onto stack */
    argframe[i] = l->args[i];

  if (l->rtype == __AVstruct)		/* push struct return address */
    __asm__("move%.l %0,%/a1" : : "g" (l->raddr));

  i = (*l->func)();			/* call function */

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
  case __AVfloat:
    if (l->flags & __AV_FREG_FLOAT_RETURN)
      RETURN(float, fp_fret);
    else
      if (l->flags & __AV_SUNCC_FLOAT_RETURN)
        RETURN(float, (float)dret);
      else
        RETURN(float, fret);
    break;
  case __AVdouble:
    if (l->flags & __AV_FREG_FLOAT_RETURN)
      RETURN(double, fp_dret);
    else
      RETURN(double, dret);
    break;
  case __AVvoidp:	RETURN(void*,		i);	break;
  case __AVstruct:
    /* NB: On m68k, all structure sizes are divisible by 2. */
    if (l->flags & __AV_REGISTER_STRUCT_RETURN)
      switch (l->rsize)
      {
	case sizeof(char):  RETURN(char,  i);	goto done; /* can't occur */
	case sizeof(short): RETURN(short, i);	goto done;
	case sizeof(int):   RETURN(int,   i);	goto done;
	case 2*sizeof(__avword):
	  ((__avword*)l->raddr)[0] = i;
	  ((__avword*)l->raddr)[1] = iret2;
	  goto done;
	default:				break;
      }
    if (l->flags & __AV_PCC_STRUCT_RETURN)
    { /* pcc struct return convention: need a  *(TYPE*)l->raddr = *(TYPE*)i;  */
      switch (l->rsize)
      {
      case sizeof(char):  RETURN(char,	*(char*)i);	break; /* can't occur */
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
    { /* normal struct return convention */ }
    break;
  default:					break;
  }
 done:
  sp += __AV_ALIST_WORDS;
  return 0;
}

#endif /*_avcall_m68k_c */
