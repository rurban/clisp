/* Tiny GCC Library
 * setjmp.h
 * Jörg Höhle, 29-Nov-92
 */

#ifndef _SETJMP_H_
#define _SETJMP_H_

/*
 * GCCBuRP uses 13
 * GCCWild uses 17
 * DICE    uses 16
 */
#define _JBLEN	13

typedef long jmp_buf[_JBLEN];

int setjmp (jmp_buf);
volatile void longjmp (jmp_buf, int);

#endif /* !_SETJMP_H_ */
