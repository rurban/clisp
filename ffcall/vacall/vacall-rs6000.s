	.file	"vacall-rs6000.c"
.toc
.csect .text[PR]
gcc2_compiled.:
__gnu_compiled_c:
	.extern vacall_function[RW]
.toc
LC..0:
	.tc vacall_function[TC],vacall_function[RW]
.csect .text[PR]
	.align 2
	.globl vacall
	.globl .vacall
.csect vacall[DS]
vacall:
	.long .vacall, TOC[tc0], 0
.csect .text[PR]
.vacall:
	mflr 0
	st 0,8(1)
	stu 1,-192(1)
	st 3,216(1)
	st 4,220(1)
	st 5,224(1)
	st 6,228(1)
	st 7,232(1)
	st 8,236(1)
	st 9,240(1)
	st 10,244(1)
	stfd 1,88(1)
	stfd 2,96(1)
	stfd 3,104(1)
	stfd 4,112(1)
	stfd 5,120(1)
	stfd 6,128(1)
	stfd 7,136(1)
	stfd 8,144(1)
	stfd 9,152(1)
	stfd 10,160(1)
	stfd 11,168(1)
	stfd 12,176(1)
	stfd 13,184(1)
	lil 9,0
	st 9,56(1)
	cal 0,216(1)
	st 0,60(1)
	st 9,64(1)
	st 9,68(1)
	cal 0,88(1)
	l 9,LC..0(2)
	cal 3,56(1)
	l 9,0(9)
	st 0,84(1)
	l 0,0(9)
	st 2,20(1)
	l 2,4(9)
	l 11,8(9)
	mtlr 0
	brl
	l 2,20(1)
	l 0,68(1)
	cmpi 1,0,0
	bc 12,6,L..3
	cmpi 1,0,1
	bc 12,6,L..41
	cmpi 1,0,2
	bc 4,6,L..6
	lbz 0,76(1)
	sli 0,0,24
	srai 3,0,24
	b L..3
L..6:
	cmpi 1,0,3
	bc 4,6,L..8
L..41:
	lbz 3,76(1)
	b L..3
L..8:
	cmpi 1,0,4
	bc 4,6,L..10
	lha 3,76(1)
	b L..3
L..10:
	cmpi 1,0,5
	bc 4,6,L..12
	lhz 3,76(1)
	b L..3
L..12:
	cmpi 1,0,6
	bc 12,6,L..42
	cmpi 1,0,7
	bc 12,6,L..42
	cmpi 1,0,8
	bc 12,6,L..42
	cmpi 1,0,9
	bc 12,6,L..42
	ai 0,0,-10
	cmpli 1,0,1
	bc 12,5,L..22
	l 3,76(1)
	l 4,80(1)
	b L..3
L..22:
	l 0,68(1)
	cmpi 1,0,12
	bc 4,6,L..24
	lfs 1,76(1)
	b L..3
L..24:
	cmpi 1,0,13
	bc 4,6,L..26
	lfd 1,76(1)
	b L..3
L..26:
	cmpi 1,0,14
	bc 4,6,L..28
L..42:
	l 3,76(1)
	b L..3
L..28:
	cmpi 1,0,15
	bc 4,6,L..3
	l 9,56(1)
	andil. 0,9,1
	bc 12,2,L..31
	l 3,64(1)
	b L..3
L..31:
	andil. 0,9,1024
	bc 12,2,L..3
	l 0,72(1)
	cmpi 1,0,1
	bc 4,6,L..34
	l 9,64(1)
	lbz 3,0(9)
	b L..3
L..34:
	cmpi 1,0,2
	bc 4,6,L..36
	l 9,64(1)
	lhz 3,0(9)
	b L..3
L..36:
	cmpi 1,0,4
	bc 4,6,L..38
	l 9,64(1)
	l 3,0(9)
	b L..3
L..38:
	cmpi 1,0,8
	bc 4,6,L..3
	l 9,64(1)
	l 3,0(9)
	l 4,4(9)
L..3:
	cal 1,192(1)
	l 0,8(1)
	mtlr 0
	br
LT..vacall:
	.long 0
	.byte 0,0,32,65,128,0,8,0
	.long 0
	.long LT..vacall-.vacall
	.short 6
	.byte "vacall"
_section_.text:
.csect .data[RW]
	.long _section_.text
