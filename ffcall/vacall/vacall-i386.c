/* vacall function for i386 CPU */

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
         *	env	__asm__("%ecx");
#endif
register void*	sp	__asm__("%esp");
register void*	sret	__asm__("%ebx");
register int	iret	__asm__("%eax");

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
        /* This code is EXTREMELY fragile!!                     */
        /* It depends on the register allocation chosen by gcc. */
        iret = ((__vaword *) &list.tmp._longlong)[0];
        asm volatile ("movl %0,%%edx" : : "g"(((__vaword *) &list.tmp._longlong)[1]));
        break;
      case __VAfloat:	asm volatile ("flds %0": : "m"(list.tmp._float)); break;
      case __VAdouble:	asm volatile ("fldl %0": : "m"(list.tmp._double)); break;
      case __VAvoidp:	iret = (long)list.tmp._ptr; break;
      case __VAstruct:
        if (list.flags & __VA_PCC_STRUCT_RETURN)
          { /* pcc struct return convention */
            iret = (long) list.raddr;
          }
        else
          { /* normal struct return convention */
            if (list.flags & __VA_REGISTER_STRUCT_RETURN)
              switch (list.rsize)
                { case sizeof(char):  iret = *(unsigned char *) list.raddr; goto done;
                  case sizeof(short): iret = *(unsigned short *) list.raddr; goto done;
                  case sizeof(int):   iret = *(unsigned int *) list.raddr; goto done;
                  case 2*sizeof(__vaword):
                    /* This code is EXTREMELY fragile!!                     */
                    /* It depends on the register allocation chosen by gcc. */
                    iret = ((__vaword *) list.raddr)[0];
                    asm volatile ("movl %0,%%edx" : : "g"(((__vaword *) list.raddr)[1]));
                    goto done;
                  default: break;
                }
            if (!(list.flags & (__VA_NEXTGCC_STRUCT_RETURN | __VA_MSVC_STRUCT_RETURN)))
              { /* We have to pop the struct return address off the stack. */
                /* Callers compiled with -fomit-frame-pointer expect this. */
                /* Return via a "ret $4" instruction. */
                /* NOTE: This is EXTREMELY fragile. It depends on the fact that
                 * no registers have to be restored from the stack. Look at the
                 * assembly code!
                 */
                sp = __builtin_frame_address(0);
                asm volatile ("ret $4");
                /*NOTREACHED*/
              }
            if (list.flags & __VA_MSVC_STRUCT_RETURN)
              { /* on MSVC, must put the structure address into %eax */
                iret = (long) list.raddr;
              }
            done: ;
          }
        break;
    }
  if (list.flags & __VA_STDCALL_CLEANUP)
    { /* Return, and at the same time pop the arguments off the stack. */
      /* Normally done through a "ret $n" instruction. */
      /* Be careful not to clobber %eax and %edx. Only %ecx can be used. */
      /* Use *__builtin_frame_address(0), since __builtin_return_address(0)
       * is buggy in gcc-2.7.2. */
      asm volatile ("movl %0,%%ecx" : : "g" (*(void**)__builtin_frame_address(0)));
      sp = (void*)list.aptr;
      asm volatile ("jmp *%ecx");
      /*NOTREACHED*/
    }
}
