/* vacall function for hppa CPU */

/*
 * Copyright 1995-1997 Bruno Haible, <haible@clisp.cons.org>
 *
 * This is free software distributed under the GNU General Public Licence
 * described in the file COPYING. Contact the author if you don't have this
 * or can't live with it. There is ABSOLUTELY NO WARRANTY, explicit or implied,
 * on this software.
 */

/*---------------------------------------------------------------------------
  HPPA Argument Passing Conventions:

  The calling conventions for anonymous functions and for explicitly named
  functions are different. Only the convention for explicitly named functions
  matters here.

  All arguments, except the first 4 words, are passed on the stack
  - growing down! - with word alignment. Doubles take two words and force
  double alignment. Structures args are passed as true structures embedded
  in the argument stack. They force double alignment and - if they don't
  fit entirely in the 4 register words - are passed in memory.
  The first 2 words are passed like this:
    %r26 = first integer arg, %r25 = second integer arg, or
    %fr4L = first float arg, %fr5L = second float arg, or
    %fr5 = double arg.
  Similarly for the next 2 words, passed in %r24 and %r23, or
                                            %fr6L and %fr7L, or
                                            %fr7.

  To return a structure, the called function copies the return value to
  the address supplied in register "%r28".
---------------------------------------------------------------------------*/

#ifndef REENTRANT
#include "vacall.h.in"
#else /* REENTRANT */
#include "vacall_r.h.in"
#endif

#ifdef REENTRANT
#define vacall __vacall_r
register struct { void (*vacall_function) (void*,va_alist); void* arg; }
         *	env	__asm__("%r29");
#endif
register void*	sret	__asm__("%r28");
register long   arg1	__asm__("%r26");
register long   arg2	__asm__("%r25");
register long   arg3	__asm__("%r24");
register long   arg4	__asm__("%r23");
register float  farg1	__asm__("%fr4"); /* fr4L */
register float  farg2	__asm__("%fr5"); /* fr5L */
register float  farg3	__asm__("%fr6"); /* fr6L */
register float  farg4	__asm__("%fr7"); /* fr7L */
register double darg1	__asm__("%fr5");
register double darg2	__asm__("%fr7");
register int	iret	__asm__("%r28");
register float	fret	__asm__("%fr4"); /* fr4L */
register double	dret	__asm__("%fr4");
register __vaword iret1	__asm__("%r28");
register __vaword iret2	__asm__("%r29");

void /* the return type is variable, not void! */
vacall (__vaword word1, __vaword word2, __vaword word3, __vaword word4,
        __vaword firstword)
{
  /* gcc-2.6.3 source says: When a parameter is passed in a register,
   * stack space is still allocated for it.
   */
  /* Note about stack offsets (see vacall-hppa.s):
   * &firstword = %r30 - 244, &word4 = %r30 - 240, ..., &word1 = %r30 - 228,
   */
  __va_alist list;
  /* Move the arguments passed in registers to their stack locations. */
  &word1; /* (&firstword)[4] = word1; */
  &word2; /* (&firstword)[3] = word2; */
  &word3; /* (&firstword)[2] = word3; */
  &word4; /* (&firstword)[1] = word4; */
  list.darg[1] = darg1;
  list.darg[0] = darg2;
  list.farg[3] = farg1;
  list.farg[2] = farg2;
  list.farg[1] = farg3;
  list.farg[0] = farg4;
  /* Prepare the va_alist. */
  list.flags = 0;
  list.aptr = (long)(&firstword + 5);
  list.raddr = (void*)0;
  list.rtype = __VAvoid;
  list.structraddr = sret;
  list.memargptr = (long)(&firstword + 1);
  list.farg_offset = (long)&list.farg[4] - list.aptr;
  list.darg_offset = (long)&list.darg[2] - list.aptr;
  /* Call vacall_function. The macros do all the rest. */
#ifndef REENTRANT
  (*vacall_function) (&list);
#else /* REENTRANT */
  (*env->vacall_function) (env->arg,&list);
#endif
  /* Put return value into proper register. */
  switch (list.rtype)
    {
      case __VAvoid:	break;
      case __VAchar:	iret = list.tmp._char; break;
      case __VAschar:	iret = list.tmp._schar; break;
      case __VAuchar:	iret = list.tmp._uchar; break;
      case __VAshort:	iret = list.tmp._short; break;
      case __VAushort:	iret = list.tmp._ushort; break;
      case __VAint:	iret = list.tmp._int; break;
      case __VAuint:	iret = list.tmp._uint; break;
      case __VAlong:	iret = list.tmp._long; break;
      case __VAulong:	iret = list.tmp._ulong; break;
      case __VAlonglong:
      case __VAulonglong:
        iret1 = ((__vaword *) &list.tmp._longlong)[0];
        iret2 = ((__vaword *) &list.tmp._longlong)[1];
        break;
      case __VAfloat:
        fret = list.tmp._float;
        iret1 = list.tmp._words[0]; /* HP cc generates a RTNVAL=GR call */
        break;
      case __VAdouble:
        dret = list.tmp._double;
        iret1 = list.tmp._words[0]; /* HP cc generates a RTNVAL=GR call */
        iret2 = list.tmp._words[1]; /* i.e. result is expected in r28,r29 */
        break;
      case __VAvoidp:	iret = (long)list.tmp._ptr; break;
      case __VAstruct:
        if (list.flags & __VA_PCC_STRUCT_RETURN)
          { /* pcc struct return convention */
            iret = (long) list.raddr;
          }
        else
          { /* normal struct return convention */
            if (list.flags & __VA_SMALL_STRUCT_RETURN)
              if (list.flags & __VA_OLDGCC_STRUCT_RETURN)
                /* gcc <= 2.6.3 returns structs of size 1,2,4 in registers. */
                switch (list.rsize)
                  {
                    case sizeof(char):  iret = *(unsigned char *) list.raddr; break;
                    case sizeof(short): iret = *(unsigned short *) list.raddr; break;
                    case sizeof(int):   iret = *(unsigned int *) list.raddr; break;
                    default:            break;
                  }
              else
                /* cc, c89 and gcc >= 2.7 return structs of size <= 8 in registers. */
                switch (list.rsize)
                  {
                    case 1:
                      iret =   ((unsigned char *) list.raddr)[0];
                      break;
                    case 2:
                      iret =  (((unsigned char *) list.raddr)[0] << 8)
                            |  ((unsigned char *) list.raddr)[1];
                      break;
                    case 3:
                      iret =  (((unsigned char *) list.raddr)[0] << 16)
                            | (((unsigned char *) list.raddr)[1] << 8)
                            |  ((unsigned char *) list.raddr)[2];
                      break;
                    case 4:
                      iret =  (((unsigned char *) list.raddr)[0] << 24)
                            | (((unsigned char *) list.raddr)[1] << 16)
                            | (((unsigned char *) list.raddr)[2] << 8)
                            |  ((unsigned char *) list.raddr)[3];
                      break;
                    case 5:
                      iret1 =  (((unsigned char *) list.raddr)[0] << 24)
                             | (((unsigned char *) list.raddr)[1] << 16)
                             | (((unsigned char *) list.raddr)[2] << 8)
                             |  ((unsigned char *) list.raddr)[3];
                      iret2 =   ((unsigned char *) list.raddr)[4];
                      break;
                    case 6:
                      iret1 =  (((unsigned char *) list.raddr)[0] << 24)
                             | (((unsigned char *) list.raddr)[1] << 16)
                             | (((unsigned char *) list.raddr)[2] << 8)
                             |  ((unsigned char *) list.raddr)[3];
                      iret2 =  (((unsigned char *) list.raddr)[4] << 8)
                             |  ((unsigned char *) list.raddr)[5];
                      break;
                    case 7:
                      iret1 =  (((unsigned char *) list.raddr)[0] << 24)
                             | (((unsigned char *) list.raddr)[1] << 16)
                             | (((unsigned char *) list.raddr)[2] << 8)
                             |  ((unsigned char *) list.raddr)[3];
                      iret2 =  (((unsigned char *) list.raddr)[4] << 16)
                             | (((unsigned char *) list.raddr)[5] << 8)
                             |  ((unsigned char *) list.raddr)[6];
                      break;
                    case 8:
                      iret1 =  (((unsigned char *) list.raddr)[0] << 24)
                             | (((unsigned char *) list.raddr)[1] << 16)
                             | (((unsigned char *) list.raddr)[2] << 8)
                             |  ((unsigned char *) list.raddr)[3];
                      iret2 =  (((unsigned char *) list.raddr)[4] << 24)
                             | (((unsigned char *) list.raddr)[5] << 16)
                             | (((unsigned char *) list.raddr)[6] << 8)
                             |  ((unsigned char *) list.raddr)[7];
                      break;
                    default:            break;
                  }
          }
        break;
    }
}
