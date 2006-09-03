	.file	"vacall-powerpc.c"
	.section	".text"
	.align 2
	.globl __vacall_r
	.type	__vacall_r, @function
__vacall_r:
	stwu 1,-208(1)
	mflr 0
	li 11,0
	stw 0,212(1)
	addi 0,1,160
	stw 0,20(1)
	addi 0,1,56
	stw 11,24(1)
	stw 0,48(1)
	lwz 0,0(13)
	stw 9,184(1)
	addi 9,1,216
	stw 3,160(1)
	mtctr 0
	stw 4,164(1)
	addi 4,1,16
	stw 9,192(1)
	stw 5,168(1)
	stw 6,172(1)
	stw 7,176(1)
	stw 8,180(1)
	stw 10,188(1)
	stw 11,28(1)
	stfd 1,56(1)
	stfd 2,64(1)
	stfd 3,72(1)
	stfd 4,80(1)
	stfd 5,88(1)
	stfd 6,96(1)
	stfd 7,104(1)
	stfd 8,112(1)
	stw 11,16(1)
	stw 11,196(1)
	lwz 3,4(13)
	bctrl
	lwz 9,28(1)
	cmpwi 0,9,0
	beq- 0,.L1
	cmpwi 0,9,1
	beq- 0,.L43
	cmpwi 0,9,2
	beq- 0,.L44
	cmpwi 0,9,3
	beq- 0,.L43
	cmpwi 0,9,4
	beq- 0,.L45
	cmpwi 0,9,5
	beq- 0,.L46
	cmpwi 0,9,6
	beq- 0,.L42
	cmpwi 0,9,7
	beq- 0,.L42
	cmpwi 0,9,8
	beq- 0,.L42
	cmpwi 0,9,9
	beq- 0,.L42
	addi 0,9,-10
	cmplwi 0,0,1
	bgt- 0,.L22
	lwz 3,40(1)
	lwz 4,44(1)
.L1:
	lwz 0,212(1)
	addi 1,1,208
	mtlr 0
	blr
.L22:
	cmpwi 0,9,12
	beq- 0,.L47
	cmpwi 0,9,13
	beq- 0,.L48
	cmpwi 0,9,14
	beq- 0,.L42
	cmpwi 0,9,15
	bne+ 0,.L1
	lwz 0,16(1)
	andi. 9,0,1
	beq- 0,.L31
	lwz 3,24(1)
	b .L1
.L31:
	andi. 9,0,1024
	beq- 0,.L1
	lwz 0,32(1)
	cmpwi 0,0,1
	beq- 0,.L49
	cmpwi 0,0,2
	beq- 0,.L50
	cmpwi 0,0,4
	beq- 0,.L51
	cmpwi 0,0,8
	bne+ 0,.L1
	lwz 9,24(1)
	lwz 4,4(9)
.L41:
	lwz 3,0(9)
	b .L1
.L51:
	lwz 9,24(1)
	b .L41
.L50:
	lwz 9,24(1)
	lhz 3,0(9)
	b .L1
.L49:
	lwz 9,24(1)
	lbz 3,0(9)
	b .L1
.L42:
	lwz 3,40(1)
	b .L1
.L48:
	lfd 1,40(1)
	b .L1
.L47:
	lfs 1,40(1)
	b .L1
.L46:
	lhz 3,40(1)
	b .L1
.L45:
	lha 3,40(1)
	b .L1
.L43:
	lbz 3,40(1)
	b .L1
.L44:
	lbz 0,40(1)
	extsb 3,0
	b .L1
	.size	__vacall_r, .-__vacall_r
	.ident	"GCC: (GNU) 3.3.3 (NetBSD nb3 20040520)"
