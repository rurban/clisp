/* vacall function for m68k CPU */

/*
 * Copyright 1995-1999 Bruno Haible, <haible@clisp.cons.org>
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
         *	env	__asm__("a0");
#endif
register void*	sret	__asm__("a1");
register int	iret	__asm__("d0");
register int	iret2	__asm__("d1");
register int	pret	__asm__("a0");	/* some compilers return pointers in a0 */
register float	fret	__asm__("d0");	/* d0 */
register double	dret	__asm__("d0");	/* d0,d1 */
register float	fp_fret	__asm__("fp0");
register double	fp_dret	__asm__("fp0");

void /* the return type is variable, not void! */
vacall (__vaword firstword)
{
  __va_alist list;
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
      case __VAfloat:
        if (list.flags & __VA_FREG_FLOAT_RETURN)
          fp_fret = list.tmp._float;
        else
          if (list.flags & __VA_SUNCC_FLOAT_RETURN)
            dret = (double)list.tmp._float;
          else
            fret = list.tmp._float;
        break;
      case __VAdouble:
        if (list.flags & __VA_FREG_FLOAT_RETURN)
          fp_dret = list.tmp._double;
        else
          dret = list.tmp._double;
        break;
      case __VAvoidp:	pret = iret = (long)list.tmp._ptr; break;
      case __VAstruct:
        /* NB: On m68k, all structure sizes are divisible by 2. */
        if (list.flags & __VA_REGISTER_STRUCT_RETURN)
          switch (list.rsize)
            { case sizeof(char):  iret = *(unsigned char *) list.raddr; goto done; /* can't occur */
              case sizeof(short): iret = *(unsigned short *) list.raddr; goto done;
              case sizeof(int):   iret = *(unsigned int *) list.raddr; goto done;
              case 2*sizeof(__vaword):
                iret  = ((__vaword *) list.raddr)[0];
                iret2 = ((__vaword *) list.raddr)[1];
                goto done;
              default:            break;
            }
        if (list.flags & __VA_PCC_STRUCT_RETURN)
          { /* pcc struct return convention */
            pret = iret = (long) list.raddr;
          }
        else
          { /* normal struct return convention */ }
       done:
        break;
    }
}
