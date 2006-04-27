/* Trampoline for powerpc64 CPU with AIX calling convention */

/*
 * Copyright 1995-2006 Bruno Haible, <bruno@clisp.org>
 *
 * This is free software distributed under the GNU General Public Licence
 * described in the file COPYING. Contact the author if you don't have this
 * or can't live with it. There is ABSOLUTELY NO WARRANTY, explicit or implied,
 * on this software.
 */

/* Available registers: r0, r12, r11, r10, r9, r8, r7, r6, r5, r4, r3. */
/* However, r0 is special in that it cannot be used as a base register. */
/* And r3...r10 should not be clobbered because they contain the first 8
 * integer arguments to the function being called. */

	.globl tramp
	.globl .tramp
tramp:
	.quad .tramp
.tramp:
/* Move <data> into <variable> */
	ld 11,0(2)	/* get <variable> */
	ld 12,8(2)	/* get <data> */
	std 12,0(11)
/* Get <function> */
	ld 12,16(2)
/*
 * gcc-2.6.3 source says:
 * A function pointer is a pointer to a data area whose first word contains
 * the actual address of the function, whose second word contains a pointer
 * to its TOC, and whose third word contains a value to place in the static
 * chain register (r11).
 */
	ld 11,16(12)	/* pass static chain in r11 */
	ld 2,8(12)	/* pass TOC in r2 */
	ld 0,0(12)	/* actual code address */
	mtctr 0
	bctr
