	.file	"vacall-rs6000.c"
.toc
.csect .text[PR]
gcc2_compiled.:
__gnu_compiled_c:
	.align 2
	.globl __vacall_r
	.globl .__vacall_r
.csect __vacall_r[DS]
__vacall_r:
	.long .__vacall_r, TOC[tc0], 0
.csect .text[PR]
.__vacall_r:
	.extern __mulh
	.extern __mull
	.extern __divss
	.extern __divus
	.extern __quoss
	.extern __quous
	mflr 0
	st 25,-28(1)
	st 26,-24(1)
	st 27,-20(1)
	st 28,-16(1)
	st 29,-12(1)
	st 30,-8(1)
	st 31,-4(1)
	st 0,8(1)
	stu 1,-232(1)
	cal 29,288(1)
	st 10,-4(29)
	st 5,-24(29)
	st 6,-20(29)
	st 7,-16(29)
	st 8,-12(29)
	st 9,-8(29)
	lil 28,0
	cal 0,256(1)
	cal 27,92(1)
	st 0,60(1)
	st 28,68(1)
	st 27,88(1)
	st 4,-28(29)
	st 3,-32(29)
	stfd 1,92(1)
	stfd 2,100(1)
	stfd 3,108(1)
	stfd 4,116(1)
	stfd 5,124(1)
	stfd 6,132(1)
	stfd 7,140(1)
	stfd 8,148(1)
	stfd 9,156(1)
	stfd 10,164(1)
	stfd 11,172(1)
	stfd 12,180(1)
	stfd 13,188(1)
	st 28,56(1)
	st 28,64(1)
	l 0,0(11)
	l 3,4(11)
	cal 4,56(1)
	mr 25,0
	st 2,20(1)
	l 26,0(25)
	l 2,4(25)
	mtlr 26
	l 11,8(25)
	brl
	l 2,20(1)
	l 9,68(1)
	cmpi 0,9,0
	mr 10,9
	bc 12,2,L..3
	cmpi 0,9,1
	bc 12,2,L..41
	cmpi 0,9,2
	bc 4,2,L..6
	lbz 0,80(1)
	sli 0,0,24
	srai 3,0,24
	b L..3
L..6:
	cmpi 0,9,3
	bc 4,2,L..8
L..41:
	lbz 3,80(1)
	b L..3
L..8:
	cmpi 0,9,4
	bc 4,2,L..10
	lha 3,80(1)
	b L..3
L..10:
	cmpi 0,9,5
	bc 4,2,L..12
	lhz 3,80(1)
	b L..3
L..12:
	cmpi 0,9,6
	bc 12,2,L..42
	cmpi 0,9,7
	bc 12,2,L..42
	cmpi 0,9,8
	bc 12,2,L..42
	cmpi 0,9,9
	bc 12,2,L..42
	cal 0,-10(9)
	cmpli 0,0,1
	bc 12,1,L..22
	l 3,80(1)
	l 4,84(1)
	b L..3
L..22:
	cmpi 0,10,12
	bc 4,2,L..24
	lfs 1,80(1)
	b L..3
L..24:
	cmpi 0,9,13
	bc 4,2,L..26
	lfd 1,80(1)
	b L..3
L..26:
	cmpi 0,9,14
	bc 4,2,L..28
L..42:
	l 3,80(1)
	b L..3
L..28:
	cmpi 0,9,15
	bc 4,2,L..3
	l 0,56(1)
	andil. 25,0,1
	bc 12,2,L..31
	l 3,64(1)
	b L..3
L..31:
	andil. 26,0,1024
	bc 12,2,L..3
	l 0,72(1)
	cmpi 0,0,1
	bc 4,2,L..34
	l 9,64(1)
	lbz 3,0(9)
	b L..3
L..34:
	cmpi 0,0,2
	bc 4,2,L..36
	l 9,64(1)
	lhz 3,0(9)
	b L..3
L..36:
	cmpi 0,0,4
	bc 4,2,L..38
	l 9,64(1)
	b L..43
L..38:
	cmpi 0,0,8
	bc 4,2,L..3
	l 9,64(1)
	l 4,4(9)
L..43:
	l 3,0(9)
L..3:
	cal 1,232(1)
	l 0,8(1)
	mtlr 0
	l 25,-28(1)
	l 26,-24(1)
	l 27,-20(1)
	l 28,-16(1)
	l 29,-12(1)
	l 30,-8(1)
	l 31,-4(1)
	br
LT..__vacall_r:
	.long 0
	.byte 0,0,32,65,128,7,8,0
	.long 0
	.long LT..__vacall_r-.__vacall_r
	.short 10
	.byte "__vacall_r"
_section_.text:
.csect .data[RW]
	.long _section_.text
