	.file	"vacall-rs6000.c"
gcc2_compiled.:
	.section	".text"
	.align 2
	.globl vacall
	.type	 vacall,@function
vacall:
	.extern __mulh
	.extern __mull
	.extern __divss
	.extern __divus
	.extern __quoss
	.extern __quous
	stwu 1,-176(1)
	mflr 0
	stw 27,156(1)
	stw 28,160(1)
	stw 29,164(1)
	stw 30,168(1)
	stw 31,172(1)
	stw 0,180(1)
	addis 11,0,vacall_function@ha
	addi 29,1,184
	lwz 12,vacall_function@l(11)
	stw 3,-32(29)
	stw 10,-4(29)
	mtlr 12
	stw 5,-24(29)
	stw 6,-20(29)
	stw 7,-16(29)
	stw 8,-12(29)
	stw 9,-8(29)
	li 0,0
	addi 11,1,152
	addi 28,1,48
	addi 3,1,8
	stw 11,12(1)
	stw 0,20(1)
	stw 28,40(1)
	stw 4,-28(29)
	stfd 1,48(1)
	stfd 2,56(1)
	stfd 3,64(1)
	stfd 4,72(1)
	stfd 5,80(1)
	stfd 6,88(1)
	stfd 7,96(1)
	stfd 8,104(1)
	stfd 9,112(1)
	stfd 10,120(1)
	stfd 11,128(1)
	stfd 12,136(1)
	stfd 13,144(1)
	stw 0,8(1)
	stw 0,16(1)
	blrl
	lwz 9,20(1)
	cmpwi 0,9,0
	mr 11,9
	bc 12,2,.L3
	cmpwi 0,9,1
	bc 12,2,.L41
	cmpwi 0,9,2
	bc 4,2,.L6
	lbz 0,32(1)
	slwi 0,0,24
	srawi 3,0,24
	b .L3
.L6:
	cmpwi 0,9,3
	bc 4,2,.L8
.L41:
	lbz 3,32(1)
	b .L3
.L8:
	cmpwi 0,9,4
	bc 4,2,.L10
	lha 3,32(1)
	b .L3
.L10:
	cmpwi 0,9,5
	bc 4,2,.L12
	lhz 3,32(1)
	b .L3
.L12:
	cmpwi 0,9,6
	bc 12,2,.L42
	cmpwi 0,9,7
	bc 12,2,.L42
	cmpwi 0,9,8
	bc 12,2,.L42
	cmpwi 0,9,9
	bc 12,2,.L42
	addi 0,9,-10
	cmplwi 0,0,1
	bc 12,1,.L22
	lwz 3,32(1)
	lwz 4,36(1)
	b .L3
.L22:
	cmpwi 0,11,12
	bc 4,2,.L24
	lfs 1,32(1)
	b .L3
.L24:
	cmpwi 0,9,13
	bc 4,2,.L26
	lfd 1,32(1)
	b .L3
.L26:
	cmpwi 0,9,14
	bc 4,2,.L28
.L42:
	lwz 3,32(1)
	b .L3
.L28:
	cmpwi 0,9,15
	bc 4,2,.L3
	lwz 0,8(1)
	andi. 27,0,1
	bc 12,2,.L31
	lwz 3,16(1)
	b .L3
.L31:
	andi. 27,0,1024
	bc 12,2,.L3
	lwz 0,24(1)
	cmpwi 0,0,1
	bc 4,2,.L34
	lwz 9,16(1)
	lbz 3,0(9)
	b .L3
.L34:
	cmpwi 0,0,2
	bc 4,2,.L36
	lwz 9,16(1)
	lhz 3,0(9)
	b .L3
.L36:
	cmpwi 0,0,4
	bc 4,2,.L38
	lwz 9,16(1)
	b .L43
.L38:
	cmpwi 0,0,8
	bc 4,2,.L3
	lwz 9,16(1)
	lwz 4,4(9)
.L43:
	lwz 3,0(9)
.L3:
	lwz 0,180(1)
	mtlr 0
	lwz 27,156(1)
	lwz 28,160(1)
	lwz 29,164(1)
	lwz 30,168(1)
	lwz 31,172(1)
	la 1,176(1)
	blr
.Lfe1:
	.size	 vacall,.Lfe1-vacall
	.ident	"GCC: (GNU) egcs-2.91.66 19990314 (egcs-1.1.2 release)"
