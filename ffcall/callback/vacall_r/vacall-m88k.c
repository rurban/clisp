/* vacall function for m88k CPU */

/*
 * Copyright 1995-1997 Bruno Haible, <haible@clisp.cons.org>
 *
 * This is free software distributed under the GNU General Public Licence
 * described in the file COPYING. Contact the author if you don't have this
 * or can't live with it. There is ABSOLUTELY NO WARRANTY, explicit or implied,
 * on this software.
 */

#ifndef REENTRANT
#include "vacall.h.in"
#else /* REENTRANT */
#include "vacall_r.h.in"
#endif

#ifdef REENTRANT
#define vacall __vacall_r
register struct { void (*vacall_function) (void*,va_alist); void* arg; }
         *	env	__asm__("r11");
#endif
register __vaword* sret	__asm__("r12");
register int	iret	__asm__("r2");
register int	iret2	__asm__("r3");
register float	fret	__asm__("r2");
register double	dret	__asm__("r2");

void /* the return type is variable, not void! */
vacall (__vaword word1, __vaword word2, __vaword word3, __vaword word4,
        __vaword word5, __vaword word6, __vaword word7, __vaword word8,
        __vaword firstword)
{
  __va_alist list;
  /* gcc-2.6.3 source says: When a parameter is passed in a register,
   * stack space is still allocated for it.
   */
  /* Move the arguments passed in registers to their stack locations. */
  /*
   * Is this correct?? I think struct arguments among the first 8 words
   * are passed on the stack, not in registers, and we shouldn't overwrite
   * them.
   */
  (&firstword)[-8] = word1;
  (&firstword)[-7] = word2;
  (&firstword)[-6] = word3;
  (&firstword)[-5] = word4;
  (&firstword)[-4] = word5;
  (&firstword)[-3] = word6;
  (&firstword)[-2] = word7;
  (&firstword)[-1] = word8;
  /* Prepare the va_alist. */
  list.flags = 0;
  list.aptr = (long)&firstword;
  list.raddr = (void*)0;
  list.rtype = __VAvoid;
  list.structraddr = sret;
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
        iret  = ((__vaword *) &list.tmp._longlong)[0];
        iret2 = ((__vaword *) &list.tmp._longlong)[1];
        break;
      case __VAfloat:	fret = list.tmp._float; break;
      case __VAdouble:	dret = list.tmp._double; break;
      case __VAvoidp:	iret = (long)list.tmp._ptr; break;
      case __VAstruct:
        if (list.flags & __VA_PCC_STRUCT_RETURN)
          { /* pcc struct return convention */
            iret = (long) list.raddr;
          }
        else
          { /* normal struct return convention */
            if (list.flags & __VA_SMALL_STRUCT_RETURN)
              switch (list.rsize)
                { case sizeof(char):  iret = *(unsigned char *) list.raddr; break;
                  case sizeof(short): iret = *(unsigned short *) list.raddr; break;
                  case sizeof(int):   iret = *(unsigned int *) list.raddr; break;
                  default:            break;
                }
          }
        break;
    }
}
