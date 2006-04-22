	.file	"vacall-powerpc64.c"
	.section	".toc","aw"
.LC0:
	.tc vacall_function[TC],vacall_function
	.section	".text"
	.align 2
	.globl __vacall
	.section	".opd","aw"
	.align 3
__vacall:
	.quad	.__vacall,.TOC.@tocbase,0
	.previous
	.size	__vacall,24
	.type	.__vacall,@function
	.globl	.__vacall
.__vacall:
	mflr 0
	ld 11,.LC0@toc(2)
	std 29,-24(1)
	std 0,16(1)
	li 0,0
	stdu 1,-400(1)
	ld 29,0(11)
	li 11,0
	std 9,496(1)
	addi 9,1,448
	stw 0,112(1)
	addi 0,1,176
	std 3,448(1)
	addi 3,1,112
	std 9,120(1)
	std 4,456(1)
	std 5,464(1)
	std 6,472(1)
	std 7,480(1)
	std 8,488(1)
	std 10,504(1)
	stw 11,136(1)
	std 0,168(1)
	stfd 1,176(1)
	stfd 2,184(1)
	stfd 3,192(1)
	stfd 4,200(1)
	stfd 5,208(1)
	stfd 6,216(1)
	stfd 7,224(1)
	stfd 8,232(1)
	stfd 9,240(1)
	stfd 10,248(1)
	stfd 11,256(1)
	stfd 12,264(1)
	stfd 13,272(1)
	std 11,128(1)
	ld 0,0(29)
	std 2,40(1)
	mtctr 0
	ld 11,16(29)
	ld 2,8(29)
	bctrl
	ld 2,40(1)
	lwz 9,136(1)
	cmpwi 7,9,0
	beq- 7,.L1
	cmpwi 7,9,1
	beq- 7,.L34
	cmpwi 7,9,2
	beq- 7,.L36
	cmpwi 7,9,3
	beq- 7,.L34
	cmpwi 7,9,4
	beq- 7,.L37
	cmpwi 7,9,5
	beq- 7,.L38
	cmpwi 7,9,6
	beq- 7,.L39
	cmpwi 7,9,7
	beq- 7,.L40
	cmpwi 7,9,8
	beq- 7,.L33
	cmpwi 7,9,9
	beq- 7,.L33
	cmpwi 7,9,10
	beq- 7,.L33
	cmpwi 7,9,11
	beq- 7,.L33
	cmpwi 7,9,12
	beq- 7,.L41
	cmpwi 7,9,13
	beq- 7,.L42
	cmpwi 7,9,14
	beq- 7,.L33
.L1:
	addi 1,1,400
	ld 0,16(1)
	ld 29,-24(1)
	mtlr 0
	blr
.L34:
	lbz 3,152(1)
	addi 1,1,400
	ld 0,16(1)
	ld 29,-24(1)
	mtlr 0
	blr
.L36:
	lbz 0,152(1)
	extsb 3,0
	addi 1,1,400
	ld 0,16(1)
	ld 29,-24(1)
	mtlr 0
	blr
.L37:
	lha 3,152(1)
	b .L1
.L38:
	lhz 3,152(1)
	b .L1
.L33:
	ld 3,152(1)
	b .L1
.L39:
	lwa 3,152(1)
	b .L1
.L40:
	lwz 3,152(1)
	b .L1
.L41:
	lfs 1,152(1)
	b .L1
.L42:
	lfd 1,152(1)
	b .L1
	.long 0
	.byte 0,0,0,1,128,3,0,0
	.size	.__vacall,.-.__vacall
	.ident	"GCC: (GNU) 3.3.3 (SuSE Linux)"
