/* Trampoline for i386 CPU */

/*
 * Copyright 1995-1997 Bruno Haible, <haible@clisp.cons.org>
 *
 * This is free software distributed under the GNU General Public Licence
 * described in the file COPYING. Contact the author if you don't have this
 * or can't live with it. There is ABSOLUTELY NO WARRANTY, explicit or implied,
 * on this software.
 */

/* Available registers: %eax, %edx, %ecx. */

.globl _tramp
_tramp:
	movl $0x73554711,%ecx
	jmp 0xbabebec0
