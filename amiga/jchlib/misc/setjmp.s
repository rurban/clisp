| GCC Library
| Jörg Höhle, 6-Jul-94
| Translated to MIT Syntax from GCCBuRP project

.text
	.even
.globl _setjmp
_setjmp:
	movel sp@(4),a0		| jmp_buf
	movel sp@,a1		| a1 is setjmp() caller's return address
	moveml #0xfefc,a0@	| a7-a1,d7-d2
	moveq #0,d0
	rts

.globl _longjmp
_longjmp:
	movel sp@(4),a0		| jmp_buf
	movel sp@(8),d0		| value
	moveml a0@,#0xfefc	| a7-a1,d7-d2, a1 is return address
	movel a1,sp@		| set saved setjmp() caller's return address
	rts
