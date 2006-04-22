#ifndef _avcall_powerpc64_c				/*-*- C -*-*/
#define _avcall_powerpc64_c
/**
  Copyright 1993 Bill Triggs, <Bill.Triggs@inrialpes.fr>
  Copyright 1995-2006 Bruno Haible, <bruno@clisp.org>
  Copyright 2000 Adam Fedor, <fedor@gnu.org>
  Copyright 2004 Paul Guyot, <pguyot@kallisys.net>

  This is free software distributed under the GNU General Public
  Licence described in the file COPYING. Contact the author if
  you don't have this or can't live with it. There is ABSOLUTELY
  NO WARRANTY, explicit or implied, on this software.
**/
/*----------------------------------------------------------------------
  !!! THIS ROUTINE MUST BE COMPILED gcc -O !!!

  Foreign function interface for a 64-bit PowerPC with gcc

  This calls a C function with an argument list built up using macros
  defined in avcall.h.

  PowerPC64 Argument Passing Conventions:

  All arguments, except the first 8 words, are passed on the stack with
  word alignment. The first 13 doubles and floats are also passed in
  floating-point-registers.
  To return a structure, the called function copies the value to space
  pointed to by its first argument, and all other arguments are shifted
  down by one.

  The AIX argument passing conventions are used:
  - the first 13 doubles and floats are passed in FP registers,
    and when they do, there is still room allocated for them in the
    argument sequence (integer regs or stack).
  - Structures are passed in the argument sequence. But structures
    containing floats or doubles are passed in FP registers?!

  Compile this routine with gcc -O (or -O2 -fno-omit-frame-pointer or -g -O)
  to get the right register variables. For other compilers use the
  pre-compiled assembler version.
  ----------------------------------------------------------------------*/
#include "avcall.h.in"

#define STACK_OFFSET 14

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
  __avword* argframe = sp + STACK_OFFSET;/* stack offset for argument list */
  int arglen = l->aptr - l->args;
  __avword i;

  for (i = 8; i < arglen; i++) /* push function args onto stack */
    argframe[i-8] = l->args[i];

  /* pass first 13 floating-point args in registers */
  arglen = l->faptr - l->fargs;
  if (arglen == 0) goto fargs0;
  else if (arglen == 1) goto fargs1;
  else if (arglen == 2) goto fargs2;
  else if (arglen == 3) goto fargs3;
  else if (arglen == 4) goto fargs4;
  else if (arglen == 5) goto fargs5;
  else if (arglen == 6) goto fargs6;
  else if (arglen == 7) goto fargs7;
  else if (arglen == 8) goto fargs8;
  else if (arglen == 9) goto fargs9;
  else if (arglen == 10) goto fargs10;
  else if (arglen == 11) goto fargs11;
  else if (arglen == 12) goto fargs12;
  else if (arglen == 13) goto fargs13;
  fargs13: farg13 = l->fargs[12];
  fargs12: farg12 = l->fargs[11];
  fargs11: farg11 = l->fargs[10];
  fargs10: farg10 = l->fargs[9];
  fargs9: farg9 = l->fargs[8];
  fargs8: farg8 = l->fargs[7];
  fargs7: farg7 = l->fargs[6];
  fargs6: farg6 = l->fargs[5];
  fargs5: farg5 = l->fargs[4];
  fargs4: farg4 = l->fargs[3];
  fargs3: farg3 = l->fargs[2];
  fargs2: farg2 = l->fargs[1];
  fargs1: farg1 = l->fargs[0];
  fargs0: ;
				/* call function, pass 8 args in registers */
  i = (*l->func)(l->args[0], l->args[1], l->args[2], l->args[3],
		 l->args[4], l->args[5], l->args[6], l->args[7]);

  /* save return value */
  if (l->rtype == __AVvoid) {
  } else
  if (l->rtype == __AVword) {
    RETURN(__avword, i);
  } else
  if (l->rtype == __AVchar) {
    RETURN(char, i);
  } else
  if (l->rtype == __AVschar) {
    RETURN(signed char, i);
  } else
  if (l->rtype == __AVuchar) {
    RETURN(unsigned char, i);
  } else
  if (l->rtype == __AVshort) {
    RETURN(short, i);
  } else
  if (l->rtype == __AVushort) {
    RETURN(unsigned short, i);
  } else
  if (l->rtype == __AVint) {
    RETURN(int, i);
  } else
  if (l->rtype == __AVuint) {
    RETURN(unsigned int, i);
  } else
  if (l->rtype == __AVlong) {
    RETURN(long, i);
  } else
  if (l->rtype == __AVulong) {
    RETURN(unsigned long, i);
  } else
  if (l->rtype == __AVlonglong) {
    RETURN(long long, i);
  } else
  if (l->rtype == __AVulonglong) {
    RETURN(unsigned long long, i);
  } else
  if (l->rtype == __AVfloat) {
    RETURN(float, fret);
  } else
  if (l->rtype == __AVdouble) {
    RETURN(double, dret);
  } else
  if (l->rtype == __AVvoidp) {
    RETURN(void*, i);
  } else
  if (l->rtype == __AVstruct) {
    /* __AV_PCC_STRUCT_RETURN and __AV_REGISTER_STRUCT_RETURN are ignored */
  }
  return 0;
}

#endif /*_avcall_powerpc64_c */
