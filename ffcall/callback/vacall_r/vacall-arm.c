/* vacall function for arm CPU */

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
         *		env	__asm__("r12");
#endif
register __vaword	iret	__asm__("r0");
register __vaword	iret2	__asm__("r1");
register float		fret	__asm__("f0");
register double		dret	__asm__("f0");

void /* the return type is variable, not void! */
vacall (__vaword word1, __vaword word2, __vaword word3, __vaword word4,
        __vaword firstword)
{
  struct { long retaddr, arg1, arg2, arg3, arg4; } args;
  __va_alist list;
  /* MAGIC ALERT!
   * This is the last struct on the stack, so that
   * &args + 1 == &return_address == &firstword - 1.
   * Look at the assembly code to convince yourself.
   */
  /* Move the arguments passed in registers to their stack locations. */
  args.retaddr = (&firstword)[-1]; /* save the return address */
  (&firstword)[-4] = word1;
  (&firstword)[-3] = word2;
  (&firstword)[-2] = word3;
  (&firstword)[-1] = word4;
  /* Prepare the va_alist. */
  list.flags = 0;
  list.aptr = (long)(&firstword - 4);
  list.raddr = (void*)0;
  list.rtype = __VAvoid;
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
        /* NB: On arm, all structure sizes are divisible by 4. */
        if (list.flags & __VA_PCC_STRUCT_RETURN)
          { /* pcc struct return convention */
            iret = (long) list.raddr;
          }
        else
          { /* normal struct return convention */
            if (list.flags & __VA_REGISTER_STRUCT_RETURN)
              switch (list.rsize)
                { case sizeof(char):  iret = *(unsigned char *) list.raddr; break; /* can't occur */
                  case sizeof(short): iret = *(unsigned short *) list.raddr; break; /* can't occur */
                  case sizeof(int):   iret = *(unsigned int *) list.raddr; break;
                  case 2*sizeof(__vaword):
                    iret  = ((__vaword *) list.raddr)[0];
                    iret2 = ((__vaword *) list.raddr)[1];
                    break;
                  default:            break;
                }
          }
        break;
    }
  (&firstword)[-1] = args.retaddr; /* restore the return address */
}
