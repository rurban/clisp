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
	stw 25,-28(1)
	stw 26,-24(1)
	stw 27,-20(1)
	stw 28,-16(1)
	stw 29,-12(1)
	stw 30,-8(1)
	stw 31,-4(1)
	stw 0,8(1)
	stwu 1,-232(1)
	addi 29,1,288
	stw 10,-4(29)
	stw 5,-24(29)
	stw 6,-20(29)
	stw 7,-16(29)
	stw 8,-12(29)
	stw 9,-8(29)
	li 28,0
	addi 0,1,256
	addi 27,1,92
	stw 0,60(1)
	stw 28,68(1)
	stw 27,88(1)
	stw 4,-28(29)
	stw 3,-32(29)
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
	stw 28,56(1)
	stw 28,64(1)
	lwz 0,0(11)
	lwz 3,4(11)
	addi 4,1,56
	mr 25,0
	stw 2,20(1)
	lwz 26,0(25)
	lwz 2,4(25)
	mtlr 26
	lwz 11,8(25)
	blrl
	lwz 2,20(1)
	lwz 9,68(1)
	cmpwi 0,9,0
	mr 10,9
	bc 12,2,L..3
	cmpwi 0,9,1
	bc 12,2,L..41
	cmpwi 0,9,2
	bc 4,2,L..6
	lbz 0,80(1)
	slwi 0,0,24
	srawi 3,0,24
	b L..3
L..6:
	cmpwi 0,9,3
	bc 4,2,L..8
L..41:
	lbz 3,80(1)
	b L..3
L..8:
	cmpwi 0,9,4
	bc 4,2,L..10
	lha 3,80(1)
	b L..3
L..10:
	cmpwi 0,9,5
	bc 4,2,L..12
	lhz 3,80(1)
	b L..3
L..12:
	cmpwi 0,9,6
	bc 12,2,L..42
	cmpwi 0,9,7
	bc 12,2,L..42
	cmpwi 0,9,8
	bc 12,2,L..42
	cmpwi 0,9,9
	bc 12,2,L..42
	addi 0,9,-10
	cmplwi 0,0,1
	bc 12,1,L..22
	lwz 3,80(1)
	lwz 4,84(1)
	b L..3
L..22:
	cmpwi 0,10,12
	bc 4,2,L..24
	lfs 1,80(1)
	b L..3
L..24:
	cmpwi 0,9,13
	bc 4,2,L..26
	lfd 1,80(1)
	b L..3
L..26:
	cmpwi 0,9,14
	bc 4,2,L..28
L..42:
	lwz 3,80(1)
	b L..3
L..28:
	cmpwi 0,9,15
	bc 4,2,L..3
	lwz 0,56(1)
	andi. 25,0,1
	bc 12,2,L..31
	lwz 3,64(1)
	b L..3
L..31:
	andi. 26,0,1024
	bc 12,2,L..3
	lwz 0,72(1)
	cmpwi 0,0,1
	bc 4,2,L..34
	lwz 9,64(1)
	lbz 3,0(9)
	b L..3
L..34:
	cmpwi 0,0,2
	bc 4,2,L..36
	lwz 9,64(1)
	lhz 3,0(9)
	b L..3
L..36:
	cmpwi 0,0,4
	bc 4,2,L..38
	lwz 9,64(1)
	b L..43
L..38:
	cmpwi 0,0,8
	bc 4,2,L..3
	lwz 9,64(1)
	lwz 4,4(9)
L..43:
	lwz 3,0(9)
L..3:
	la 1,232(1)
	lwz 0,8(1)
	mtlr 0
	lwz 25,-28(1)
	lwz 26,-24(1)
	lwz 27,-20(1)
	lwz 28,-16(1)
	lwz 29,-12(1)
	lwz 30,-8(1)
	lwz 31,-4(1)
	blr
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
