	.file	"proto.c"
.toc
.csect .text[PR]
gcc2_compiled.:
__gnu_compiled_c:
	.align 2
	.globl tramp
	.globl .tramp
.csect tramp[DS]
tramp:
	.long .tramp, TOC[tc0], 0
.csect .text[PR]
.tramp:
	mflr 0
	st 0,8(1)
	stu 1,-56(1)
	liu 9,0x1234
	oril 9,9,22136
	liu 0,0x7355
	oril 0,0,18193
	st 0,0(9)
	liu 9,0xbabe
	oril 9,9,48832
	l 0,0(9)
	st 2,20(1)
	l 2,4(9)
	l 11,8(9)
	mtlr 0
	brl
	l 2,20(1)
	cal 1,56(1)
	l 0,8(1)
	mtlr 0
	br
LT..tramp:
	.long 0
	.byte 0,0,32,65,128,0,0,0
	.long LT..tramp-.tramp
	.short 5
	.byte "tramp"
	.align 2
	.globl jump
	.globl .jump
.csect jump[DS]
jump:
	.long .jump, TOC[tc0], 0
.csect .text[PR]
.jump:
	liu 0,0xbabe
	oril 0,0,48832
	mtctr 0
	bctr
LT..jump:
	.long 0
	.byte 0,0,32,64,0,0,0,0
	.long LT..jump-.jump
	.short 4
	.byte "jump"
_section_.text:
.csect .data[RW]
	.long _section_.text
