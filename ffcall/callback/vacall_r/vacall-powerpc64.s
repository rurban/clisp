	.file	"vacall-powerpc64.c"
	.section	".toc","aw"
	.section	".text"
	.align 2
	.p2align 4,,15
	.globl __vacall_r
	.section	".opd","aw"
	.align 3
__vacall_r:
	.quad	.L.__vacall_r,.TOC.@tocbase,0
	.previous
	.type	__vacall_r, @function
.L.__vacall_r:
	mflr 0
	std 29,-24(1)
	li 29,0
	std 0,16(1)
	stdu 1,-400(1)
	li 0,0
	stw 29,136(1)
	stw 0,112(1)
	addi 0,1,448
	std 9,496(1)
	std 3,448(1)
	std 4,456(1)
	std 29,128(1)
	std 5,464(1)
	std 6,472(1)
	std 7,480(1)
	std 8,488(1)
	std 10,504(1)
	std 0,120(1)
	stfd 1,176(1)
	stfd 2,184(1)
	addi 9,1,176
	stfd 3,192(1)
	stfd 4,200(1)
	stfd 5,208(1)
	stfd 6,216(1)
	addi 4,1,112
	std 9,168(1)
	stfd 7,224(1)
	stfd 8,232(1)
	stfd 9,240(1)
	stfd 10,248(1)
	stfd 11,256(1)
	stfd 12,264(1)
	stfd 13,272(1)
	ld 9,0(11)
	ld 3,8(11)
	ld 0,0(9)
	std 2,40(1)
	mtctr 0
	ld 11,16(9)
	ld 2,8(9)
	bctrl
	ld 2,40(1)
	lwz 0,136(1)
	cmpdi 7,0,0
	beq 7,.L31
	cmpwi 7,0,1
	beq 7,.L32
	cmpwi 7,0,2
	beq 7,.L35
	cmpwi 7,0,3
	beq 7,.L32
	cmpwi 7,0,4
	beq 7,.L36
	cmpwi 7,0,5
	beq 7,.L37
	cmpwi 7,0,6
	beq 7,.L38
	cmpwi 7,0,7
	beq 7,.L39
	cmpwi 7,0,8
	beq 7,.L33
	cmpwi 7,0,9
	beq 7,.L33
	cmpwi 7,0,10
	beq 7,.L33
	cmpwi 7,0,11
	beq 7,.L33
	cmpwi 7,0,12
	beq 7,.L40
	cmpwi 7,0,13
	beq 7,.L41
	cmpwi 7,0,14
	beq 7,.L33
	.p2align 4,,15
.L31:
	addi 1,1,400
	ld 0,16(1)
	ld 29,-24(1)
	mtlr 0
	blr
	.p2align 4,,15
.L32:
	lbz 3,152(1)
	addi 1,1,400
	ld 0,16(1)
	ld 29,-24(1)
	mtlr 0
	blr
.L36:
	lha 3,152(1)
	b .L31
	.p2align 4,,15
.L35:
	lbz 0,152(1)
	addi 1,1,400
	ld 29,-24(1)
	extsb 3,0
	ld 0,16(1)
	mtlr 0
	blr
.L33:
	ld 3,152(1)
	b .L31
.L37:
	lhz 3,152(1)
	b .L31
.L38:
	lwa 3,152(1)
	b .L31
.L39:
	lwz 3,152(1)
	b .L31
.L40:
	lfs 1,152(1)
	b .L31
.L41:
	lfd 1,152(1)
	b .L31
	.long 0
	.byte 0,0,0,1,128,3,0,0
	.size	__vacall_r,.-.L.__vacall_r
	.ident	"GCC: (GNU) 4.0.2"
