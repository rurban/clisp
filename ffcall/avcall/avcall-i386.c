#ifndef _avcall_i386_c				/*-*- C -*-*/
#define _avcall_i386_c
/**
  Copyright 1993 Bill Triggs, <Bill.Triggs@inrialpes.fr>

  This is free software distributed under the GNU General Public
  Licence described in the file COPYING. Contact the author if
  you don't have this or can't live with it. There is ABSOLUTELY
  NO WARRANTY, explicit or implied, on this software.
**/
/*----------------------------------------------------------------------
  !!! THIS ROUTINE MUST BE COMPILED gcc -O -fno-omit-frame-pointer !!!

  Foreign function interface for a Linux i386/486 with gcc.

  This calls a C function with an argument list built up using macros
  defined in av_call.h.

  i386 Argument Passing Conventions:

  All arguments are passed on the stack with word alignment. Doubles take
  two words. Structure args are passed as true structures embedded in the
  argument stack. Float and double returns often come from FPU registers.

  To return a structure, the called function copies the value to space
  pointed to by its first argument, and all other arguments are shifted
  down by one. On NeXTstep, however, the called function copies the return
  value to the address supplied in register "%ebx". Gcc without
  -fpcc-struct-return returns <= 4 byte structures as integers.

  Compile this routine with gcc -O (or -O2 -fno-omit-frame-pointer or -g -O)
  to get the right register variables. For other compilers use the
  pre-compiled assembler version.

  -fomit-frame-pointer is forbidden because when calling structure returning
  functions (the "i = (*l->func)();" line below) the called functions pops
  the return value container pointer from the stack: "ret $4" instead of
  "ret". (See gcc-2.6.3 macro RETURN_POPS_ARGS.) From our point of view, %esp
  gets magically incremented. A workaround would be to push the return value
  container pointer using an __asm__("pushl %0" : : : ...) instruction.
  Similarly, when calling functions with `stdcall' linkage, %esp also gets
  incremented: all arguments (including the return value container pointer)
  are popped from the stack.
  ----------------------------------------------------------------------*/
#include "avcall.h.in"

#define RETURN(TYPE,VAL)	(*(TYPE*)l->raddr = (TYPE)(VAL))

int
__builtin_avcall(av_alist* l)
{
  register __avword*	sp	__asm__("sp");	/* C names for registers */
/*register __avword	iret	__asm__("eax"); */
  register __avword	iret2	__asm__("edx");

  __avword* argframe = (sp -= __AV_ALIST_WORDS); /* make room for argument list */
  int arglen = l->aptr - l->args;
  __avword i;

  for (i = 0; i < arglen; i++)		/* push function args onto stack */
    argframe[i] = l->args[i];

  /* struct return address */
  if ((l->flags & __AV_NEXTGCC_STRUCT_RETURN) && (l->rtype == __AVstruct))
    __asm__("movl %0,%%ebx" : : "g" (l->raddr) : "bx" /* %ebx */);

  switch (l->rtype)			/* call function */
  {
  case __AVfloat:
    *(float*)l->raddr = (*(float(*)())l->func)();
    return 0;
  case __AVdouble:
    *(double*)l->raddr = (*(double(*)())l->func)();
    return 0;
  default:
    i = (*l->func)();
    break;
  }
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
  case __AVfloat:	/* see above */			break;
  case __AVdouble:	/* see above */			break;
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

#endif /*_avcall_i386_c */
